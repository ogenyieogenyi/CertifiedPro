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
