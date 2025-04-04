;; title: CertifiedPro
;; version: 1.0
;; summary: Professional certification verification system
;; description: A decentralized platform for verifying professional certifications with skills marketplace, endorsement system, and time-locked credentials

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-expired (err u104))

;; Data maps
;; Store professional certifications
(define-map certifications
  { id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    issuer: (string-ascii 100),
    issue-date: uint,
    expiry-date: uint,
    skills: (list 10 (string-ascii 50)),
    active: bool
  }
)

;; Track certification IDs by owner
(define-map user-certifications
  { owner: principal }
  { cert-ids: (list 50 uint) }
)

;; Endorsements for certifications
(define-map endorsements
  { cert-id: uint, endorser: principal }
  {
    rating: uint,
    comment: (string-ascii 200),
    timestamp: uint
  }
)

;; Skills marketplace
(define-map skills-marketplace
  { skill: (string-ascii 50) }
  { professionals: (list 100 principal) }
)

;; Data vars
(define-data-var next-id uint u1)

;; Public functions
;; Issue a new certification
(define-public (issue-certification 
    (title (string-ascii 100))
    (issuer (string-ascii 100))
    (issue-date uint)
    (expiry-date uint)
    (skills (list 10 (string-ascii 50)))
  )
  (let
    (
      (cert-id (var-get next-id))
      (user-certs (default-to { cert-ids: (list) } (map-get? user-certifications { owner: tx-sender })))
    )
    ;; Increment the ID counter
    (var-set next-id (+ cert-id u1))
    
    ;; Store the certification
    (map-set certifications
      { id: cert-id }
      {
        owner: tx-sender,
        title: title,
        issuer: issuer,
        issue-date: issue-date,
        expiry-date: expiry-date,
        skills: skills,
        active: true
      }
    )
    
    ;; Update user's certification list
    (map-set user-certifications
      { owner: tx-sender }
      { cert-ids: (unwrap-panic (as-max-len? (append (get cert-ids user-certs) cert-id) u50)) }
    )
    
    ;; Add professional to skills marketplace for each skill
    (map add-to-marketplace skills)
    
    (ok cert-id)
  )
)

;; Add endorsement to a certification
(define-public (endorse-certification (cert-id uint) (rating uint) (comment (string-ascii 200)))
  (let
    (
      (cert (unwrap! (map-get? certifications { id: cert-id }) (err err-not-found)))
      (timestamp (get-stacks-block-info? time (- stacks-block-height u1)))
    )
    ;; Check if certification exists and is active
    (asserts! (get active cert) (err err-expired))
    ;; Ensure endorser is not the owner
    (asserts! (not (is-eq tx-sender (get owner cert))) (err err-unauthorized))
    ;; Ensure rating is between 1 and 5
    (asserts! (and (>= rating u1) (<= rating u5)) (err err-unauthorized))
    
    ;; Store the endorsement
    (map-set endorsements
      { cert-id: cert-id, endorser: tx-sender }
      {
        rating: rating,
        comment: comment,
        timestamp: (default-to u0 timestamp)
      }
    )
    
    (ok true)
  )
)

;; Revoke a certification (owner or issuer can revoke)
(define-public (revoke-certification (cert-id uint))
  (let
    (
      (cert (unwrap! (map-get? certifications { id: cert-id }) (err err-not-found)))
    )
    ;; Only the owner can revoke their certification
    (asserts! (is-eq tx-sender (get owner cert)) (err err-unauthorized))
    
    ;; Update the certification to inactive
    (map-set certifications
      { id: cert-id }
      (merge cert { active: false })
    )
    
    (ok true)
  )
)

;; Read only functions
;; Get certification details
(define-read-only (get-certification (cert-id uint))
  (map-get? certifications { id: cert-id })
)

;; Get all certifications for a user
(define-read-only (get-user-certifications (user principal))
  (map-get? user-certifications { owner: user })
)

;; Get endorsement for a certification
(define-read-only (get-endorsement (cert-id uint) (endorser principal))
  (map-get? endorsements { cert-id: cert-id, endorser: endorser })
)

;; Check if a certification is valid (not expired and active)
(define-read-only (is-certification-valid (cert-id uint))
  (let
    (
      (cert (unwrap! (map-get? certifications { id: cert-id }) false))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (and
      (get active cert)
      (> (get expiry-date cert) current-time)
    )
  )
)

;; Get professionals with a specific skill
(define-read-only (get-professionals-by-skill (skill (string-ascii 50)))
  (default-to { professionals: (list) } (map-get? skills-marketplace { skill: skill }))
)

;; Private functions
;; Helper function to add a professional to the skills marketplace
(define-private (add-to-marketplace (skill (string-ascii 50)))
  (let
    (
      (marketplace-entry (default-to { professionals: (list) } (map-get? skills-marketplace { skill: skill })))
      (professionals (get professionals marketplace-entry))
    )
    (if (or
          (is-some (index-of professionals tx-sender))
          (>= (len professionals) u99)
        )
      true
      (map-set skills-marketplace
        { skill: skill }
        { professionals: (unwrap-panic (as-max-len? (append professionals tx-sender) u100)) }
      )
    )
  )
)



;; Add new error codes
(define-constant err-transfer-not-allowed (err u105))

;; Add transfer history tracking
(define-map certification-transfers
  { cert-id: uint }
  {
    previous-owner: principal,
    new-owner: principal,
    transfer-date: uint,
    transfer-reason: (string-ascii 100)
  }
)

;; Transfer certification ownership
(define-public (transfer-certification 
    (cert-id uint) 
    (new-owner principal) 
    (transfer-reason (string-ascii 100))
  )
  (let
    (
      (cert (unwrap! (map-get? certifications { id: cert-id }) (err err-not-found)))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Verify ownership and active status
    (asserts! (is-eq tx-sender (get owner cert)) (err err-unauthorized))
    (asserts! (get active cert) (err err-expired))
    
    ;; Record transfer history
    (map-set certification-transfers
      { cert-id: cert-id }
      {
        previous-owner: tx-sender,
        new-owner: new-owner,
        transfer-date: current-time,
        transfer-reason: transfer-reason
      }
    )
    
    ;; Update certification ownership
    (map-set certifications
      { id: cert-id }
      (merge cert { owner: new-owner })
    )
    
    (ok true)
  )
)


;; Add category management
(define-map certification-categories
  { category-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    parent-category: (optional uint)
  }
)

(define-data-var next-category-id uint u1)

;; Add category to certification
(define-map certification-category-mapping
  { cert-id: uint }
  { category-id: uint }
)

;; Create new category
(define-public (create-category 
    (name (string-ascii 50)) 
    (description (string-ascii 200))
    (parent-category (optional uint))
  )
  (let
    ((category-id (var-get next-category-id)))
    
    (var-set next-category-id (+ category-id u1))
    
    (map-set certification-categories
      { category-id: category-id }
      {
        name: name,
        description: description,
        parent-category: parent-category
      }
    )
    
    (ok category-id)
  )
)

;; Assign category to certification
(define-public (assign-category (cert-id uint) (category-id uint))
  (let
    ((cert (unwrap! (map-get? certifications { id: cert-id }) (err err-not-found))))
    
    (asserts! (is-eq tx-sender (get owner cert)) (err err-unauthorized))
    
    (map-set certification-category-mapping
      { cert-id: cert-id }
      { category-id: category-id }
    )
    
    (ok true)
  )
)


;; Achievement badges tracking
(define-map achievement-badges
  { badge-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    requirement-count: uint
  }
)

(define-map user-badges
  { user: principal }
  { earned-badges: (list 50 uint) }
)

(define-data-var next-badge-id uint u1)

;; Create new achievement badge
(define-public (create-achievement-badge 
    (name (string-ascii 50))
    (description (string-ascii 200))
    (requirement-count uint)
  )
  (let
    ((badge-id (var-get next-badge-id)))
    
    (var-set next-badge-id (+ badge-id u1))
    
    (map-set achievement-badges
      { badge-id: badge-id }
      {
        name: name,
        description: description,
        requirement-count: requirement-count
      }
    )
    
    (ok badge-id)
  )
)

;; Award badge to user
(define-public (award-badge (user principal) (badge-id uint))
  (let
    ((current-badges (default-to { earned-badges: (list) } (map-get? user-badges { user: user }))))
    
    (map-set user-badges
      { user: user }
      { earned-badges: (unwrap-panic (as-max-len? (append (get earned-badges current-badges) badge-id) u50)) }
    )
    
    (ok true)
  )
)


;; Track verification attempts
(define-map verification-history
  { cert-id: uint }
  {
    verifications: (list 100 {
      verifier: principal,
      timestamp: uint,
      result: bool
    })
  }
)

;; Record verification attempt
(define-public (verify-certification (cert-id uint))
  (let
    (
      (is-valid (is-certification-valid cert-id))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (current-history (default-to { verifications: (list) } 
        (map-get? verification-history { cert-id: cert-id })))
    )
    
    (map-set verification-history
      { cert-id: cert-id }
      {
        verifications: (unwrap-panic (as-max-len? 
          (append (get verifications current-history)
            { verifier: tx-sender, timestamp: current-time, result: is-valid }
          ) u100))
      }
    )
    
    (ok is-valid)
  )
)


;; Track skill endorsements
(define-map skill-endorsements
  { skill: (string-ascii 50), professional: principal }
  {
    endorsement-count: uint,
    total-rating: uint,
    level: uint
  }
)

;; Endorse a professional's skill
(define-public (endorse-skill 
    (skill (string-ascii 50)) 
    (professional principal)
    (rating uint)
  )
  (let
    (
      (current-endorsement (default-to 
        { endorsement-count: u0, total-rating: u0, level: u0 }
        (map-get? skill-endorsements { skill: skill, professional: professional })))
    )
    
    (asserts! (and (>= rating u1) (<= rating u5)) (err err-unauthorized))
    
    (map-set skill-endorsements
      { skill: skill, professional: professional }
      {
        endorsement-count: (+ (get endorsement-count current-endorsement) u1),
        total-rating: (+ (get total-rating current-endorsement) rating),
        level: (/ (+ (get total-rating current-endorsement) rating) 
                 (+ (get endorsement-count current-endorsement) u1))
      }
    )
    
    (ok true)
  )
)


;; Track renewal history
(define-map certification-renewals
  { cert-id: uint }
  {
    renewal-count: uint,
    last-renewal-date: uint,
    next-renewal-date: uint
  }
)

;; Renew certification
(define-public (renew-certification 
    (cert-id uint)
    (new-expiry-date uint)
  )
  (let
    (
      (cert (unwrap! (map-get? certifications { id: cert-id }) (err err-not-found)))
      (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
      (current-renewal (default-to 
        { renewal-count: u0, last-renewal-date: u0, next-renewal-date: u0 }
        (map-get? certification-renewals { cert-id: cert-id })))
    )
    
    (asserts! (is-eq tx-sender (get owner cert)) (err err-unauthorized))
    (asserts! (> new-expiry-date current-time) (err err-unauthorized))
    
    ;; Update certification expiry
    (map-set certifications
      { id: cert-id }
      (merge cert { expiry-date: new-expiry-date })
    )
    
    ;; Update renewal history
    (map-set certification-renewals
      { cert-id: cert-id }
      {
        renewal-count: (+ (get renewal-count current-renewal) u1),
        last-renewal-date: current-time,
        next-renewal-date: new-expiry-date
      }
    )
    
    (ok true)
  )
)


;; Index certifications by various criteria
(define-map certification-indices
  {
    issuer: (string-ascii 100),
    category: (string-ascii 50),
    issue-year: uint
  }
  { cert-ids: (list 100 uint) }
)

;; Add certification to indices
(define-private (index-certification (cert-id uint))
  (let
    ((cert (unwrap! (map-get? certifications { id: cert-id }) false)))
    
    (map-set certification-indices
      {
        issuer: (get issuer cert),
        category: "default",
        issue-year: (/ (get issue-date cert) u31536000)
      }
      {
        cert-ids: (unwrap-panic (as-max-len? 
          (append 
            (get cert-ids (default-to { cert-ids: (list) } 
              (map-get? certification-indices 
                {
                  issuer: (get issuer cert),
                  category: "default",
                  issue-year: (/ (get issue-date cert) u31536000)
                }
              )
            ))
            cert-id
          ) 
          u100))
      }
    )
    
    true
  )
)