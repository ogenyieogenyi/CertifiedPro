;; Skills Assessment and Quiz System
;; Enables creation and management of competency-based assessments for professional certifications

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-assessment-inactive (err u203))
(define-constant err-insufficient-score (err u204))
(define-constant err-cooldown-active (err u205))
(define-constant err-max-attempts-reached (err u206))
(define-constant err-invalid-answer (err u207))
(define-constant err-already-completed (err u208))

;; Data variables
(define-data-var next-assessment-id uint u1)
(define-data-var next-attempt-id uint u1)

;; Assessment structure with questions and metadata
(define-map assessments
    { assessment-id: uint }
    {
        creator: principal,
        title: (string-ascii 100),
        skill-area: (string-ascii 50),
        passing-score: uint,
        max-attempts: uint,
        cooldown-period: uint,
        active: bool,
        created-at: uint
    }
)

;; Individual questions within assessments
(define-map assessment-questions
    { assessment-id: uint, question-index: uint }
    {
        question-text: (string-ascii 300),
        option-a: (string-ascii 100),
        option-b: (string-ascii 100),
        option-c: (string-ascii 100),
        option-d: (string-ascii 100),
        correct-answer: uint,
        points: uint
    }
)

;; Track question count per assessment
(define-map assessment-metadata
    { assessment-id: uint }
    {
        question-count: uint,
        total-points: uint
    }
)

;; Assessment attempts by users
(define-map assessment-attempts
    { attempt-id: uint }
    {
        assessment-id: uint,
        candidate: principal,
        score: uint,
        max-score: uint,
        passed: bool,
        completed-at: uint,
        attempt-number: uint
    }
)

;; Track user's assessment history
(define-map user-attempts
    { candidate: principal, assessment-id: uint }
    {
        total-attempts: uint,
        highest-score: uint,
        last-attempt-time: uint,
        passed: bool
    }
)

;; User answers for each attempt
(define-map attempt-answers
    { attempt-id: uint, question-index: uint }
    { selected-answer: uint }
)

;; Create a new skills assessment
(define-public (create-assessment 
    (title (string-ascii 100))
    (skill-area (string-ascii 50))
    (passing-score uint)
    (max-attempts uint)
    (cooldown-hours uint)
)
    (let 
        (
            (assessment-id (var-get next-assessment-id))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        
        (var-set next-assessment-id (+ assessment-id u1))
        
        (map-set assessments
            { assessment-id: assessment-id }
            {
                creator: tx-sender,
                title: title,
                skill-area: skill-area,
                passing-score: passing-score,
                max-attempts: max-attempts,
                cooldown-period: (* cooldown-hours u3600),
                active: true,
                created-at: current-time
            }
        )
        
        (map-set assessment-metadata
            { assessment-id: assessment-id }
            { question-count: u0, total-points: u0 }
        )
        
        (ok assessment-id)
    )
)

;; Add a question to an assessment
(define-public (add-question
    (assessment-id uint)
    (question-text (string-ascii 300))
    (option-a (string-ascii 100))
    (option-b (string-ascii 100))
    (option-c (string-ascii 100))
    (option-d (string-ascii 100))
    (correct-answer uint)
    (points uint)
)
    (let
        (
            (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) (err err-not-found)))
            (metadata (unwrap! (map-get? assessment-metadata { assessment-id: assessment-id }) (err err-not-found)))
            (question-index (get question-count metadata))
        )
        
        ;; Only assessment creator can add questions
        (asserts! (is-eq tx-sender (get creator assessment)) (err err-unauthorized))
        
        ;; Validate correct answer is between 1-4
        (asserts! (and (>= correct-answer u1) (<= correct-answer u4)) (err err-invalid-answer))
        
        ;; Add the question
        (map-set assessment-questions
            { assessment-id: assessment-id, question-index: question-index }
            {
                question-text: question-text,
                option-a: option-a,
                option-b: option-b,
                option-c: option-c,
                option-d: option-d,
                correct-answer: correct-answer,
                points: points
            }
        )
        
        ;; Update metadata
        (map-set assessment-metadata
            { assessment-id: assessment-id }
            {
                question-count: (+ question-index u1),
                total-points: (+ (get total-points metadata) points)
            }
        )
        
        (ok question-index)
    )
)

;; Start taking an assessment
(define-public (start-assessment (assessment-id uint))
    (let
        (
            (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) (err err-not-found)))
            (user-history (default-to 
                { total-attempts: u0, highest-score: u0, last-attempt-time: u0, passed: false }
                (map-get? user-attempts { candidate: tx-sender, assessment-id: assessment-id })))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
            (attempt-id (var-get next-attempt-id))
        )
        
        ;; Check if assessment is active
        (asserts! (get active assessment) (err err-assessment-inactive))
        
        ;; Check if user hasn't already passed
        (asserts! (not (get passed user-history)) (err err-already-completed))
        
        ;; Check max attempts
        (asserts! (< (get total-attempts user-history) (get max-attempts assessment)) (err err-max-attempts-reached))
        
        ;; Check cooldown period
        (asserts! (or 
            (is-eq (get total-attempts user-history) u0)
            (> current-time (+ (get last-attempt-time user-history) (get cooldown-period assessment))))
            (err err-cooldown-active))
        
        (var-set next-attempt-id (+ attempt-id u1))
        
        ;; Create attempt record
        (map-set assessment-attempts
            { attempt-id: attempt-id }
            {
                assessment-id: assessment-id,
                candidate: tx-sender,
                score: u0,
                max-score: u0,
                passed: false,
                completed-at: u0,
                attempt-number: (+ (get total-attempts user-history) u1)
            }
        )
        
        (ok attempt-id)
    )
)

;; Submit answer for a question during an attempt
(define-public (submit-answer 
    (attempt-id uint)
    (question-index uint)
    (selected-answer uint)
)
    (let
        (
            (attempt (unwrap! (map-get? assessment-attempts { attempt-id: attempt-id }) (err err-not-found)))
        )
        
        ;; Only the candidate can submit answers
        (asserts! (is-eq tx-sender (get candidate attempt)) (err err-unauthorized))
        
        ;; Check that attempt is not yet completed
        (asserts! (is-eq (get completed-at attempt) u0) (err err-already-completed))
        
        ;; Validate answer choice (1-4)
        (asserts! (and (>= selected-answer u1) (<= selected-answer u4)) (err err-invalid-answer))
        
        ;; Store the answer
        (map-set attempt-answers
            { attempt-id: attempt-id, question-index: question-index }
            { selected-answer: selected-answer }
        )
        
        (ok true)
    )
)

;; Complete and score an assessment attempt
(define-public (complete-assessment (attempt-id uint))
    (let
        (
            (attempt (unwrap! (map-get? assessment-attempts { attempt-id: attempt-id }) (err err-not-found)))
            (assessment-id (get assessment-id attempt))
            (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) (err err-not-found)))
            (metadata (unwrap! (map-get? assessment-metadata { assessment-id: assessment-id }) (err err-not-found)))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        
        ;; Only the candidate can complete their attempt
        (asserts! (is-eq tx-sender (get candidate attempt)) (err err-unauthorized))
        
        ;; Check that attempt is not yet completed
        (asserts! (is-eq (get completed-at attempt) u0) (err err-already-completed))
        
        ;; Calculate score
        (let
            (
                (score-result (calculate-score attempt-id (get question-count metadata)))
                (total-score (get score score-result))
                (max-possible (get max-score score-result))
                (passed (>= total-score (get passing-score assessment)))
                (user-history (default-to 
                    { total-attempts: u0, highest-score: u0, last-attempt-time: u0, passed: false }
                    (map-get? user-attempts { candidate: tx-sender, assessment-id: assessment-id })))
            )
            
            ;; Update attempt with final score
            (map-set assessment-attempts
                { attempt-id: attempt-id }
                (merge attempt {
                    score: total-score,
                    max-score: max-possible,
                    passed: passed,
                    completed-at: current-time
                })
            )
            
            ;; Update user history
            (map-set user-attempts
                { candidate: tx-sender, assessment-id: assessment-id }
                {
                    total-attempts: (+ (get total-attempts user-history) u1),
                    highest-score: (if (> total-score (get highest-score user-history)) total-score (get highest-score user-history)),
                    last-attempt-time: current-time,
                    passed: (or passed (get passed user-history))
                }
            )
            
            (ok { score: total-score, max-score: max-possible, passed: passed })
        )
    )
)

;; Calculate score for an assessment attempt
(define-private (calculate-score (attempt-id uint) (question-count uint))
    (fold calculate-question-score 
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
        { attempt-id: attempt-id, score: u0, max-score: u0, question-count: question-count }
    )
)

;; Helper function to calculate score for individual questions
(define-private (calculate-question-score 
    (question-index uint)
    (acc { attempt-id: uint, score: uint, max-score: uint, question-count: uint })
)
    (if (< question-index (get question-count acc))
        (let
            (
                (attempt-id (get attempt-id acc))
                (attempt (unwrap-panic (map-get? assessment-attempts { attempt-id: attempt-id })))
                (assessment-id (get assessment-id attempt))
                (question (map-get? assessment-questions { assessment-id: assessment-id, question-index: question-index }))
                (user-answer (map-get? attempt-answers { attempt-id: attempt-id, question-index: question-index }))
            )
            
            (match question
                q (match user-answer
                    answer (let
                        (
                            (is-correct (is-eq (get selected-answer answer) (get correct-answer q)))
                            (points (get points q))
                        )
                        {
                            attempt-id: attempt-id,
                            score: (+ (get score acc) (if is-correct points u0)),
                            max-score: (+ (get max-score acc) points),
                            question-count: (get question-count acc)
                        }
                    )
                    ;; No answer provided
                    {
                        attempt-id: attempt-id,
                        score: (get score acc),
                        max-score: (+ (get max-score acc) (get points q)),
                        question-count: (get question-count acc)
                    }
                )
                ;; Question not found
                acc
            )
        )
        acc
    )
)

;; Check if user has passed a specific assessment
(define-read-only (has-passed-assessment (candidate principal) (assessment-id uint))
    (default-to false 
        (get passed (map-get? user-attempts { candidate: candidate, assessment-id: assessment-id })))
)

;; Get assessment details
(define-read-only (get-assessment (assessment-id uint))
    (map-get? assessments { assessment-id: assessment-id })
)

;; Get user's assessment history
(define-read-only (get-user-assessment-history (candidate principal) (assessment-id uint))
    (map-get? user-attempts { candidate: candidate, assessment-id: assessment-id })
)

;; Get attempt details
(define-read-only (get-attempt (attempt-id uint))
    (map-get? assessment-attempts { attempt-id: attempt-id })
)

;; Toggle assessment active status (creator only)
(define-public (toggle-assessment-status (assessment-id uint))
    (let
        (
            (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) (err err-not-found)))
        )
        
        (asserts! (is-eq tx-sender (get creator assessment)) (err err-unauthorized))
        
        (map-set assessments
            { assessment-id: assessment-id }
            (merge assessment { active: (not (get active assessment)) })
        )
        
        (ok (not (get active assessment)))
    )
)
