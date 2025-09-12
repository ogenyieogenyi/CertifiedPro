;; CPD (Continuing Professional Development) Tracking System
;; Enables professionals to track ongoing learning and maintain certification requirements

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-input (err u303))
(define-constant err-insufficient-credits (err u304))
(define-constant err-activity-expired (err u305))
(define-constant err-already-validated (err u306))
(define-constant err-plan-inactive (err u307))

;; Data variables
(define-data-var next-activity-id uint u1)
(define-data-var next-plan-id uint u1)
(define-data-var next-validator-id uint u1)
(define-data-var next-activity-type-id uint u0)

;; CPD activity types and their base credit values
(define-map activity-types
    { type-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        base-credits-per-hour: uint,
        max-credits-per-year: uint,
        requires-validation: bool
    }
)

;; Professional learning plans
(define-map learning-plans
    { plan-id: uint }
    {
        professional: principal,
        certification-id: uint,
        target-credits: uint,
        plan-period-months: uint,
        created-at: uint,
        status: (string-ascii 20),
        current-credits: uint
    }
)

;; CPD activities logged by professionals
(define-map cpd-activities
    { activity-id: uint }
    {
        professional: principal,
        plan-id: uint,
        activity-type: uint,
        title: (string-ascii 100),
        description: (string-ascii 300),
        duration-hours: uint,
        credits-earned: uint,
        completion-date: uint,
        evidence-url: (optional (string-ascii 200)),
        validated: bool,
        validator: (optional principal),
        validation-date: (optional uint)
    }
)

;; Professional CPD summaries
(define-map professional-cpd-summary
    { professional: principal }
    {
        total-activities: uint,
        total-credits: uint,
        last-activity-date: uint,
        active-plans: uint
    }
)

;; Authorized CPD validators
(define-map cpd-validators
    { validator-id: uint }
    {
        validator: principal,
        name: (string-ascii 100),
        specialization: (string-ascii 50),
        authorized-by: principal,
        active: bool,
        validated-count: uint
    }
)

;; Initialize default activity types
(define-public (initialize-activity-types)
    (begin
        (try! (create-activity-type "Training Course" "Formal training courses and workshops" u1 u40 false))
        (try! (create-activity-type "Conference" "Professional conferences and seminars" u1 u30 false))
        (try! (create-activity-type "Webinar" "Online educational webinars" u1 u20 false))
        (try! (create-activity-type "Self-Study" "Independent study and research" u1 u25 false))
        (try! (create-activity-type "Mentoring" "Providing or receiving mentorship" u2 u20 true))
        (ok true)
    )
)

;; Create new activity type (admin only)
(define-public (create-activity-type 
    (name (string-ascii 50))
    (description (string-ascii 200))
    (base-credits uint)
    (max-credits uint)
    (requires-validation bool)
)
    (let ((type-id (var-get next-activity-type-id)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-owner-only)
            (var-set next-activity-type-id (+ type-id u1))
            (map-set activity-types
                { type-id: type-id }
                {
                    name: name,
                    description: description,
                    base-credits-per-hour: base-credits,
                    max-credits-per-year: max-credits,
                    requires-validation: requires-validation
                }
            )
            (ok type-id)
        )
    )
)

;; Create learning plan
(define-public (create-learning-plan 
    (certification-id uint)
    (target-credits uint)
    (plan-period-months uint)
)
    (let 
        (
            (plan-id (var-get next-plan-id))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        (begin
            (asserts! (> target-credits u0) err-invalid-input)
            (asserts! (and (>= plan-period-months u1) (<= plan-period-months u36)) err-invalid-input)
            
            (var-set next-plan-id (+ plan-id u1))
            
            (map-set learning-plans
                { plan-id: plan-id }
                {
                    professional: tx-sender,
                    certification-id: certification-id,
                    target-credits: target-credits,
                    plan-period-months: plan-period-months,
                    created-at: current-time,
                    status: "active",
                    current-credits: u0
                }
            )
            
            ;; Update professional summary
            (let ((current-summary (default-to 
                    { total-activities: u0, total-credits: u0, last-activity-date: u0, active-plans: u0 }
                    (map-get? professional-cpd-summary { professional: tx-sender }))))
                (map-set professional-cpd-summary
                    { professional: tx-sender }
                    (merge current-summary 
                        { active-plans: (+ (get active-plans current-summary) u1) }))
            )
            
            (ok plan-id)
        )
    )
)

;; Log CPD activity
(define-public (log-cpd-activity
    (plan-id uint)
    (activity-type-id uint)
    (title (string-ascii 100))
    (description (string-ascii 300))
    (duration-hours uint)
    (completion-date uint)
    (evidence-url (optional (string-ascii 200)))
)
    (let
        (
            (activity-id (var-get next-activity-id))
            (plan (unwrap! (map-get? learning-plans { plan-id: plan-id }) err-not-found))
            (activity-type (unwrap! (map-get? activity-types { type-id: activity-type-id }) err-not-found))
            (credits-earned (* duration-hours (get base-credits-per-hour activity-type)))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        (begin
            (asserts! (is-eq tx-sender (get professional plan)) err-unauthorized)
            (asserts! (is-eq (get status plan) "active") err-plan-inactive)
            (asserts! (> duration-hours u0) err-invalid-input)
            (asserts! (<= completion-date current-time) err-invalid-input)
            
            (var-set next-activity-id (+ activity-id u1))
            
            ;; Create activity record
            (map-set cpd-activities
                { activity-id: activity-id }
                {
                    professional: tx-sender,
                    plan-id: plan-id,
                    activity-type: activity-type-id,
                    title: title,
                    description: description,
                    duration-hours: duration-hours,
                    credits-earned: credits-earned,
                    completion-date: completion-date,
                    evidence-url: evidence-url,
                    validated: (not (get requires-validation activity-type)),
                    validator: none,
                    validation-date: none
                }
            )
            
            ;; Update learning plan progress
            (map-set learning-plans
                { plan-id: plan-id }
                (merge plan 
                    { current-credits: (+ (get current-credits plan) credits-earned) })
            )
            
            ;; Update professional summary
            (let ((current-summary (default-to 
                    { total-activities: u0, total-credits: u0, last-activity-date: u0, active-plans: u0 }
                    (map-get? professional-cpd-summary { professional: tx-sender }))))
                (map-set professional-cpd-summary
                    { professional: tx-sender }
                    (merge current-summary {
                        total-activities: (+ (get total-activities current-summary) u1),
                        total-credits: (+ (get total-credits current-summary) credits-earned),
                        last-activity-date: completion-date
                    }))
            )
            
            (ok activity-id)
        )
    )
)

;; Validate CPD activity (by authorized validator)
(define-public (validate-activity (activity-id uint) (approved bool))
    (let
        (
            (activity (unwrap! (map-get? cpd-activities { activity-id: activity-id }) err-not-found))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        (begin
            (asserts! (is-authorized-validator tx-sender) err-unauthorized)
            (asserts! (not (get validated activity)) err-already-validated)
            
            (map-set cpd-activities
                { activity-id: activity-id }
                (merge activity {
                    validated: approved,
                    validator: (some tx-sender),
                    validation-date: (some current-time)
                })
            )
            
            ;; Update validator stats
            (update-validator-stats tx-sender)
            
            (ok approved)
        )
    )
)

;; Add authorized validator (admin only)
(define-public (add-cpd-validator 
    (validator principal)
    (name (string-ascii 100))
    (specialization (string-ascii 50))
)
    (let ((validator-id (var-get next-validator-id)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-owner-only)
            
            (var-set next-validator-id (+ validator-id u1))
            
            (map-set cpd-validators
                { validator-id: validator-id }
                {
                    validator: validator,
                    name: name,
                    specialization: specialization,
                    authorized-by: tx-sender,
                    active: true,
                    validated-count: u0
                }
            )
            
            (ok validator-id)
        )
    )
)

;; Complete learning plan
(define-public (complete-learning-plan (plan-id uint))
    (let
        (
            (plan (unwrap! (map-get? learning-plans { plan-id: plan-id }) err-not-found))
        )
        (begin
            (asserts! (is-eq tx-sender (get professional plan)) err-unauthorized)
            (asserts! (is-eq (get status plan) "active") err-plan-inactive)
            (asserts! (>= (get current-credits plan) (get target-credits plan)) err-insufficient-credits)
            
            (map-set learning-plans
                { plan-id: plan-id }
                (merge plan { status: "completed" })
            )
            
            (ok true)
        )
    )
)

;; Private helper functions
(define-private (is-authorized-validator (validator principal))
    (or
        (match (map-get? cpd-validators { validator-id: u0 })
            v (and (is-eq (get validator v) validator) (get active v))
            false)
        (match (map-get? cpd-validators { validator-id: u1 })
            v (and (is-eq (get validator v) validator) (get active v))
            false)
        (match (map-get? cpd-validators { validator-id: u2 })
            v (and (is-eq (get validator v) validator) (get active v))
            false)
    )
)

(define-private (update-validator-stats (validator principal))
    ;; Simplified: Find and update the validator's stats
    (begin
        (match (map-get? cpd-validators { validator-id: u0 })
            v0 (if (is-eq (get validator v0) validator)
                    (map-set cpd-validators { validator-id: u0 } 
                        (merge v0 { validated-count: (+ (get validated-count v0) u1) }))
                    false)
            false)
        (match (map-get? cpd-validators { validator-id: u1 })
            v1 (if (is-eq (get validator v1) validator)
                    (map-set cpd-validators { validator-id: u1 } 
                        (merge v1 { validated-count: (+ (get validated-count v1) u1) }))
                    false)
            false)
        true
    )
)

;; Read-only functions
(define-read-only (get-learning-plan (plan-id uint))
    (map-get? learning-plans { plan-id: plan-id })
)

(define-read-only (get-cpd-activity (activity-id uint))
    (map-get? cpd-activities { activity-id: activity-id })
)

(define-read-only (get-professional-summary (professional principal))
    (map-get? professional-cpd-summary { professional: professional })
)

(define-read-only (get-activity-type (type-id uint))
    (map-get? activity-types { type-id: type-id })
)

(define-read-only (calculate-plan-progress (plan-id uint))
    (match (map-get? learning-plans { plan-id: plan-id })
        plan (let ((progress-percentage (/ (* (get current-credits plan) u100) (get target-credits plan))))
            (ok {
                current-credits: (get current-credits plan),
                target-credits: (get target-credits plan),
                progress-percentage: progress-percentage,
                completed: (>= (get current-credits plan) (get target-credits plan))
            }))
        err-not-found)
)

(define-read-only (get-cpd-validator (validator-id uint))
    (map-get? cpd-validators { validator-id: validator-id })
)
