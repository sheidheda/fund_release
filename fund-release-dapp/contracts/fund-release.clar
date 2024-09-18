;; Crowdfunding Smart Contract

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_INITIALIZED (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_FUNDRAISING_ENDED (err u103))
(define-constant ERR_GOAL_NOT_REACHED (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_INVALID_DURATION (err u107))

;; Data Variables
(define-data-var project-owner (optional principal) none)
(define-data-var fundraising-goal uint u0)
(define-data-var funds-raised uint u0)
(define-data-var current-milestone uint u0)
(define-data-var votes-for uint u0)
(define-data-var votes-against uint u0)
(define-data-var total-contributors uint u0)
(define-data-var fundraising-end-height uint u0)
(define-data-var project-state (string-ascii 20) "not_started")

;; Maps
(define-map donor-contributions principal uint)
(define-map milestones uint {description: (string-utf8 256), amount: uint})

;; Private Functions
(define-private (is-project-owner)
  (is-eq (some tx-sender) (var-get project-owner))
)

(define-private (is-fundraising-active)
  (and
    (is-eq (var-get project-state) "active")
    (<= block-height (var-get fundraising-end-height))
  )
)

;; Public Functions
(define-public (initialize-project (goal uint) (duration uint))
  (begin
    (asserts! (is-none (var-get project-owner)) ERR_ALREADY_INITIALIZED)
    (asserts! (> goal u0) ERR_INVALID_AMOUNT)
    (asserts! (and (> duration u0) (<= duration u52560)) ERR_INVALID_DURATION)
    (var-set project-owner (some tx-sender))
    (var-set fundraising-goal goal)
    (var-set fundraising-end-height (+ block-height duration))
    (var-set project-state "active")
    (ok true)
  )
)

(define-public (contribute (amount uint))
  (let (
    (current-contribution (default-to u0 (map-get? donor-contributions tx-sender)))
  )
    (asserts! (is-fundraising-active) ERR_FUNDRAISING_ENDED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= (+ (var-get funds-raised) amount) (var-get fundraising-goal)) ERR_GOAL_NOT_REACHED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set funds-raised (+ (var-get funds-raised) amount))
    (map-set donor-contributions tx-sender (+ current-contribution amount))
    (if (is-eq current-contribution u0)
      (var-set total-contributors (+ (var-get total-contributors) u1))
      true
    )
    (ok true)
  )
)

(define-public (vote (approve bool))
  (let ((contribution (default-to u0 (map-get? donor-contributions tx-sender))))
    (asserts! (> contribution u0) ERR_NOT_FOUND)
    (asserts! (is-eq (var-get project-state) "voting") ERR_NOT_AUTHORIZED)
    (if approve
      (var-set votes-for (+ (var-get votes-for) contribution))
      (var-set votes-against (+ (var-get votes-against) contribution))
    )
    (ok true)
  )
)

(define-public (start-vote)
  (begin
    (asserts! (is-project-owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (var-get project-state) "active") ERR_NOT_AUTHORIZED)
    (var-set project-state "voting")
    (var-set votes-for u0)
    (var-set votes-against u0)
    (ok true)
  )
)

(define-public (end-vote)
  (begin
    (asserts! (is-project-owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (var-get project-state) "voting") ERR_NOT_AUTHORIZED)
    (let ((total-votes (+ (var-get votes-for) (var-get votes-against))))
      (asserts! (> total-votes u0) ERR_NOT_FOUND)
      (if (> (var-get votes-for) (var-get votes-against))
        (begin
          (var-set current-milestone (+ (var-get current-milestone) u1))
          (var-set project-state "active")
          (ok true)
        )
        (begin
          (var-set project-state "active")
          (err u108)  ;; ERR_VOTE_FAILED
        )
      )
    )
  )
)

(define-public (add-milestone (description (string-utf8 256)) (amount uint))
  (begin
    (asserts! (is-project-owner) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= (len description) u256) (err u109))  ;; ERR_INVALID_DESCRIPTION
    (map-set milestones (var-get current-milestone) {description: description, amount: amount})
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (begin
    (asserts! (is-project-owner) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (var-get funds-raised)) ERR_INSUFFICIENT_FUNDS)
    (as-contract (stx-transfer? amount tx-sender (unwrap! (var-get project-owner) ERR_NOT_FOUND)))
  )
)

(define-public (refund)
  (let ((contribution (default-to u0 (map-get? donor-contributions tx-sender))))
    (asserts! (and
      (> block-height (var-get fundraising-end-height))
      (< (var-get funds-raised) (var-get fundraising-goal))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (> contribution u0) ERR_NOT_FOUND)
    (map-delete donor-contributions tx-sender)
    (as-contract (stx-transfer? contribution tx-sender tx-sender))
  )
)

;; Read-only Functions
(define-read-only (get-project-details)
  (ok {
    owner: (var-get project-owner),
    goal: (var-get fundraising-goal),
    raised: (var-get funds-raised),
    end-height: (var-get fundraising-end-height),
    state: (var-get project-state),
    current-milestone: (var-get current-milestone)
  })
)

(define-read-only (get-contribution (donor principal))
  (ok (default-to u0 (map-get? donor-contributions donor)))
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones milestone-id)
)