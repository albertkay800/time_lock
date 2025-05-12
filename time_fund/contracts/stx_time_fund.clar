;; title: TimeFund
;; version: 2.1.0
;; summary: A time-locked collaborative fund management system
;; description: A smart contract that enables multiple participants to pool their STX tokens
;;             with time-based release conditions, milestone tracking, and democratic governance.
;;             Includes weighted voting, validator verification systems, and emergency recovery.

;; token definitions
(define-fungible-token fund-token)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant MIN_LOCK_PERIOD u1440)  ;; approximately 10 days at 10 min/block
(define-constant VOTE_THRESHOLD u75)      ;; 75% majority required
(define-constant MIN_DEPOSIT u1000000)    ;; 1 STX = 1000000 uSTX
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-INVALID-AMOUNT (err u2))
(define-constant ERR-MILESTONE-NOT-FOUND (err u3))
(define-constant ERR-LOCK-PERIOD-NOT-MET (err u4))
(define-constant ERR-ALREADY-VOTED (err u5))
(define-constant ERR-NO-PARTICIPANT (err u6))
(define-constant ERR-NO-EMERGENCY (err u7))
(define-constant ERR-INSUFFICIENT-VOTES (err u8))

;; data vars
(define-data-var total-pool uint u0)
(define-data-var participant-count uint u0)
(define-data-var release-votes uint u0)
(define-data-var last-milestone-id uint u0)
(define-data-var emergency-state bool false)
(define-data-var emergency-votes uint u0)
(define-data-var emergency-threshold uint u90) ;; 90% needed for emergency
(define-data-var current-block-height uint u0)

;; Increment block height function
(define-public (increment-block-height)
    (begin
        (var-set current-block-height (+ (var-get current-block-height) u1))
        (ok true)))

;; data maps
(define-map participants 
    {participant: principal}  
    {amount: uint,            
     join-time: uint,         
     voting-power: uint,      
     has-voted: bool})       

(define-map milestones
    {milestone-id: uint}     
    {description: (string-ascii 256),  
     required-votes: uint,             
     completed: bool,                  
     completion-time: (optional uint)})

(define-map validators
    {validator: principal}
    {active: bool})

;; public functions
(define-public (deposit (amount uint))
    (let ((current-block (var-get current-block-height)))
        (if (>= amount MIN_DEPOSIT)
            (begin
                (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
                ;; Batch state updates to reduce operations
                (let ((new-voting-power (calculate-voting-power amount)))
                    (map-set participants 
                        {participant: tx-sender}
                        {amount: amount,
                         join-time: current-block,
                         voting-power: new-voting-power,
                         has-voted: false})
                    (var-set total-pool (+ (var-get total-pool) amount))
                    (var-set participant-count (+ (var-get participant-count) u1)))
                (ok true))
            ERR-INVALID-AMOUNT)))

(define-public (withdraw (amount uint))
    (let ((participant-info (unwrap! (map-get? participants {participant: tx-sender}) 
                                   ERR-NO-PARTICIPANT)))
        (if (and
            (can-release-funds)
            (<= amount (get amount participant-info)))
            (begin
                (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
                (ok true))
            ERR-NOT-AUTHORIZED)))

(define-public (vote-for-release)
    (let ((participant-info (unwrap! (map-get? participants {participant: tx-sender}) 
                                   ERR-NO-PARTICIPANT))
          (current-block (var-get current-block-height)))
        (if (and
            (not (get has-voted participant-info))
            (lock-period-passed (get join-time participant-info) current-block))
            (begin
                (map-set participants
                    {participant: tx-sender}
                    (merge participant-info {has-voted: true}))
                (var-set release-votes 
                    (+ (var-get release-votes) 
                       (get voting-power participant-info)))
                (ok true))
            ERR-ALREADY-VOTED)))

(define-public (batch-vote-process (milestone-ids (list 10 uint)))
    (let ((participant-info (unwrap! (map-get? participants {participant: tx-sender}) 
                                   ERR-NO-PARTICIPANT))
          (current-block (var-get current-block-height)))
        (asserts! (not (get has-voted participant-info)) ERR-ALREADY-VOTED)
        (asserts! (lock-period-passed (get join-time participant-info) current-block) ERR-LOCK-PERIOD-NOT-MET)
        (begin
            (map-set participants
                {participant: tx-sender}
                (merge participant-info {has-voted: true}))
            (var-set release-votes (+ (var-get release-votes) 
                                    (get voting-power participant-info)))
            (ok true))))
            
(define-public (add-milestone (description (string-ascii 256)) 
                            (required-votes uint))
    (let ((new-milestone-id (+ (var-get last-milestone-id) u1))
          (current-block (var-get current-block-height)))
        (if (is-validator tx-sender)
            (begin
                (map-set milestones
                    {milestone-id: new-milestone-id}
                    {description: description,
                     required-votes: required-votes,
                     completed: false,
                     completion-time: none})
                (var-set last-milestone-id new-milestone-id)
                (ok new-milestone-id))
            ERR-NOT-AUTHORIZED)))

(define-public (complete-milestone (milestone-id uint))
    (let (
        (milestone (unwrap! (map-get? milestones {milestone-id: milestone-id})
                         ERR-MILESTONE-NOT-FOUND))
        (current-votes (var-get release-votes))
        (current-block (var-get current-block-height))
        )
        (asserts! (is-validator tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (>= current-votes (get required-votes milestone)) ERR-INSUFFICIENT-VOTES)
        (map-set milestones
            {milestone-id: milestone-id}
            (merge milestone 
                  {completed: true,
                   completion-time: (some current-block)}))
        (ok true)))

;; Emergency Recovery Functions
(define-public (initiate-emergency)
    (let ((participant-info (unwrap! (map-get? participants {participant: tx-sender}) 
                                   ERR-NO-PARTICIPANT)))
        (if (and
            (>= (get voting-power participant-info) u20) ;; Must have significant stake
            (not (var-get emergency-state)))
            (begin
                (var-set emergency-state true)
                (var-set emergency-votes (get voting-power participant-info))
                (ok true))
            ERR-NOT-AUTHORIZED)))

(define-public (vote-emergency)
        (let ((participant-info (unwrap! (map-get? participants {participant: tx-sender}) 
                                    ERR-NO-PARTICIPANT)))
            (if (and 
                (var-get emergency-state)
                (not (get has-voted participant-info)))
                (begin
                    (var-set emergency-votes (+ (var-get emergency-votes) 
                                            (get voting-power participant-info)))
                    (ok true))
                ERR-NO-EMERGENCY)))

;; read only functions
(define-read-only (get-participant-info (participant principal))
    (map-get? participants {participant: participant}))

(define-read-only (get-milestone-info (milestone-id uint))
    (map-get? milestones {milestone-id: milestone-id}))

(define-read-only (get-total-pool)
    (var-get total-pool))

(define-read-only (get-vote-count)
    (var-get release-votes))

(define-read-only (can-release-funds)
    (>= (var-get release-votes)
        (* (var-get participant-count) VOTE_THRESHOLD)))

(define-read-only (get-emergency-status)
    {is-active: (var-get emergency-state),
     current-votes: (var-get emergency-votes),
     threshold: (var-get emergency-threshold)})

(define-read-only (get-current-block-height)
    (var-get current-block-height))
     
;; private functions
(define-private (is-validator (account principal))
    (default-to 
        false
        (get active (map-get? validators {validator: account}))))

(define-private (calculate-voting-power (amount uint))
    (/ (* amount u100) MIN_DEPOSIT))

(define-private (lock-period-passed (join-time uint) (current-block uint))
    (>= current-block (+ join-time MIN_LOCK_PERIOD)))

(define-private (get-milestone-count)
    (var-get last-milestone-id))