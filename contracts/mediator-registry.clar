;; Mediator Registry Contract
;; Manages mediator registration, training verification, and cultural competency tracking

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-MEDIATOR-NOT-FOUND (err u101))
(define-constant ERR-MEDIATOR-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-TRAINING (err u103))
(define-constant ERR-MEDIATOR-SUSPENDED (err u104))
(define-constant ERR-INVALID-SPECIALIZATION (err u105))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Mediator specializations
(define-constant SPEC-FAMILY u1)
(define-constant SPEC-COMMERCIAL u2)
(define-constant SPEC-WORKPLACE u3)
(define-constant SPEC-COMMUNITY u4)
(define-constant SPEC-CROSS-CULTURAL u5)

;; Training levels
(define-constant TRAINING-BASIC u1)
(define-constant TRAINING-ADVANCED u2)
(define-constant TRAINING-EXPERT u3)

;; Mediator profile data map
(define-map mediator-profiles
  { mediator: principal }
  {
    active: bool,
    training-level: uint,
    specializations: (list 10 uint),
    cultural-competencies: (list 20 (string-ascii 50)),
    languages: (list 10 (string-ascii 20)),
    total-cases: uint,
    success-rate: uint,
    average-rating: uint,
    registration-block: uint,
    last-active: uint,
    suspended: bool,
    suspension-reason: (optional (string-ascii 200))
  }
)

;; Mediator availability tracking
(define-map mediator-availability
  { mediator: principal }
  {
    available: bool,
    max-concurrent-cases: uint,
    current-cases: uint,
    preferred-schedule: (string-ascii 100),
    response-time-hours: uint
  }
)

;; Training completion tracking
(define-map training-records
  { mediator: principal, training-type: (string-ascii 50) }
  {
    completed: bool,
    completion-date: uint,
    certificate-hash: (string-ascii 64),
    expiry-date: uint,
    trainer: principal
  }
)

;; Public functions

;; Register as a mediator
(define-public (register-mediator
  (training-level uint)
  (specializations (list 10 uint))
  (cultural-competencies (list 20 (string-ascii 50)))
  (languages (list 10 (string-ascii 20)))
  (max-concurrent-cases uint)
  (preferred-schedule (string-ascii 100))
  (response-time-hours uint))
  (let (
    (mediator tx-sender)
    (current-block stacks-block-height)
  )
    ;; Check if mediator already exists
    (asserts! (is-none (map-get? mediator-profiles { mediator: mediator })) ERR-MEDIATOR-ALREADY-EXISTS)

    ;; Validate training level
    (asserts! (and (>= training-level TRAINING-BASIC) (<= training-level TRAINING-EXPERT)) ERR-INSUFFICIENT-TRAINING)

    ;; Validate specializations
    (asserts! (fold validate-specialization specializations true) ERR-INVALID-SPECIALIZATION)

    ;; Create mediator profile
    (map-set mediator-profiles
      { mediator: mediator }
      {
        active: true,
        training-level: training-level,
        specializations: specializations,
        cultural-competencies: cultural-competencies,
        languages: languages,
        total-cases: u0,
        success-rate: u0,
        average-rating: u0,
        registration-block: current-block,
        last-active: current-block,
        suspended: false,
        suspension-reason: none
      }
    )

    ;; Set availability
    (map-set mediator-availability
      { mediator: mediator }
      {
        available: true,
        max-concurrent-cases: max-concurrent-cases,
        current-cases: u0,
        preferred-schedule: preferred-schedule,
        response-time-hours: response-time-hours
      }
    )

    (ok mediator)
  )
)

;; Update mediator availability
(define-public (update-availability (available bool) (max-concurrent-cases uint) (response-time-hours uint))
  (let (
    (mediator tx-sender)
    (current-availability (unwrap! (map-get? mediator-availability { mediator: mediator }) ERR-MEDIATOR-NOT-FOUND))
  )
    (asserts! (is-some (map-get? mediator-profiles { mediator: mediator })) ERR-MEDIATOR-NOT-FOUND)

    (map-set mediator-availability
      { mediator: mediator }
      (merge current-availability {
        available: available,
        max-concurrent-cases: max-concurrent-cases,
        response-time-hours: response-time-hours
      })
    )

    (ok true)
  )
)

;; Record training completion
(define-public (record-training-completion
  (mediator principal)
  (training-type (string-ascii 50))
  (certificate-hash (string-ascii 64))
  (expiry-date uint))
  (let (
    (current-block stacks-block-height)
  )
    ;; Only contract owner or authorized trainers can record training
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)

    (map-set training-records
      { mediator: mediator, training-type: training-type }
      {
        completed: true,
        completion-date: current-block,
        certificate-hash: certificate-hash,
        expiry-date: expiry-date,
        trainer: tx-sender
      }
    )

    (ok true)
  )
)

;; Update mediator statistics after case completion
(define-public (update-mediator-stats
  (mediator principal)
  (case-successful bool)
  (rating uint))
  (let (
    (current-profile (unwrap! (map-get? mediator-profiles { mediator: mediator }) ERR-MEDIATOR-NOT-FOUND))
    (current-availability (unwrap! (map-get? mediator-availability { mediator: mediator }) ERR-MEDIATOR-NOT-FOUND))
    (new-total-cases (+ (get total-cases current-profile) u1))
    (new-success-rate (if case-successful
      (/ (* (+ (* (get success-rate current-profile) (get total-cases current-profile)) u100) u100) new-total-cases)
      (/ (* (get success-rate current-profile) (get total-cases current-profile)) new-total-cases)))
    (new-average-rating (/ (+ (* (get average-rating current-profile) (get total-cases current-profile)) rating) new-total-cases))
  )
    ;; Only dispute resolution contract can update stats
    (asserts! (is-eq contract-caller .dispute-resolution) ERR-NOT-AUTHORIZED)

    ;; Update profile statistics
    (map-set mediator-profiles
      { mediator: mediator }
      (merge current-profile {
        total-cases: new-total-cases,
        success-rate: new-success-rate,
        average-rating: new-average-rating,
        last-active: stacks-block-height
      })
    )

    ;; Decrease current cases count
    (map-set mediator-availability
      { mediator: mediator }
      (merge current-availability {
        current-cases: (if (> (get current-cases current-availability) u0)
          (- (get current-cases current-availability) u1)
          u0)
      })
    )

    (ok true)
  )
)

;; Assign mediator to case (increase current cases)
(define-public (assign-mediator-to-case (mediator principal))
  (let (
    (current-availability (unwrap! (map-get? mediator-availability { mediator: mediator }) ERR-MEDIATOR-NOT-FOUND))
    (current-profile (unwrap! (map-get? mediator-profiles { mediator: mediator }) ERR-MEDIATOR-NOT-FOUND))
  )
    ;; Only dispute resolution contract can assign mediators
    (asserts! (is-eq contract-caller .dispute-resolution) ERR-NOT-AUTHORIZED)

    ;; Check if mediator is available and not suspended
    (asserts! (get available current-availability) ERR-MEDIATOR-NOT-FOUND)
    (asserts! (not (get suspended current-profile)) ERR-MEDIATOR-SUSPENDED)

    ;; Check capacity
    (asserts! (< (get current-cases current-availability) (get max-concurrent-cases current-availability)) ERR-MEDIATOR-NOT-FOUND)

    ;; Update current cases
    (map-set mediator-availability
      { mediator: mediator }
      (merge current-availability {
        current-cases: (+ (get current-cases current-availability) u1)
      })
    )

    (ok true)
  )
)

;; Suspend mediator (admin function)
(define-public (suspend-mediator (mediator principal) (reason (string-ascii 200)))
  (let (
    (current-profile (unwrap! (map-get? mediator-profiles { mediator: mediator }) ERR-MEDIATOR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)

    (map-set mediator-profiles
      { mediator: mediator }
      (merge current-profile {
        suspended: true,
        suspension-reason: (some reason)
      })
    )

    (ok true)
  )
)

;; Read-only functions

;; Get mediator profile
(define-read-only (get-mediator-profile (mediator principal))
  (map-get? mediator-profiles { mediator: mediator })
)

;; Get mediator availability
(define-read-only (get-mediator-availability (mediator principal))
  (map-get? mediator-availability { mediator: mediator })
)

;; Get training record
(define-read-only (get-training-record (mediator principal) (training-type (string-ascii 50)))
  (map-get? training-records { mediator: mediator, training-type: training-type })
)

;; Check if mediator is qualified for specialization
(define-read-only (is-qualified-for-specialization (mediator principal) (specialization uint))
  (match (map-get? mediator-profiles { mediator: mediator })
    profile (is-some (index-of (get specializations profile) specialization))
    false
  )
)

;; Find available mediators by specialization
(define-read-only (find-available-mediators-by-spec (specialization uint) (max-results uint))
  ;; This is a simplified version - in practice, you'd need a more sophisticated matching system
  (ok specialization)
)

;; Private functions

;; Validate specialization
(define-private (validate-specialization (spec uint) (valid bool))
  (and valid (and (>= spec SPEC-FAMILY) (<= spec SPEC-CROSS-CULTURAL)))
)
