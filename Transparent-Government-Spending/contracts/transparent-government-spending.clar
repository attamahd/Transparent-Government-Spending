;; Transparent Government Spending Smart Contract
;; Real-time tracking of public fund allocation and usage

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_INVALID_DEPARTMENT (err u103))
(define-constant ERR_INVALID_PROJECT (err u104))
(define-constant ERR_PROJECT_NOT_FOUND (err u105))
(define-constant ERR_TRANSACTION_NOT_FOUND (err u106))
(define-constant ERR_INVALID_STATUS (err u107))

;; Contract owner/administrator
(define-constant CONTRACT_OWNER tx-sender)

;; Data structures
(define-map government-departments
  { dept-id: uint }
  {
    name: (string-ascii 50),
    head: principal,
    total-budget: uint,
    spent-amount: uint,
    active: bool
  }
)

(define-map projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    department-id: uint,
    budget: uint,
    spent: uint,
    status: (string-ascii 20),
    manager: principal,
    start-block: uint,
    end-block: uint,
    description: (string-ascii 200)
  }
)

(define-map expenditures
  { tx-id: uint }
  {
    project-id: uint,
    amount: uint,
    recipient: principal,
    purpose: (string-ascii 150),
    timestamp: uint,
    approved-by: principal,
    status: (string-ascii 15),
    receipt-hash: (optional (buff 32))
  }
)

(define-map budget-allocations
  { allocation-id: uint }
  {
    department-id: uint,
    fiscal-year: uint,
    allocated-amount: uint,
    allocation-date: uint,
    allocated-by: principal
  }
)

;; Data variables
(define-data-var next-dept-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var next-tx-id uint u1)
(define-data-var next-allocation-id uint u1)
(define-data-var total-treasury uint u0)
(define-data-var total-allocated uint u0)
(define-data-var total-spent uint u0)

;; Authorization lists
(define-map authorized-officials principal bool)
(define-map department-managers { dept-id: uint, manager: principal } bool)

;; Initialize contract
(begin
  (map-set authorized-officials CONTRACT_OWNER true)
)

;; Read-only functions
(define-read-only (get-treasury-balance)
  (var-get total-treasury)
)

(define-read-only (get-total-allocated)
  (var-get total-allocated)
)

(define-read-only (get-total-spent)
  (var-get total-spent)
)

(define-read-only (get-department (dept-id uint))
  (map-get? government-departments { dept-id: dept-id })
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-expenditure (tx-id uint))
  (map-get? expenditures { tx-id: tx-id })
)

(define-read-only (get-budget-allocation (allocation-id uint))
  (map-get? budget-allocations { allocation-id: allocation-id })
)

(define-read-only (is-authorized (user principal))
  (default-to false (map-get? authorized-officials user))
)

(define-read-only (is-department-manager (dept-id uint) (manager principal))
  (default-to false (map-get? department-managers { dept-id: dept-id, manager: manager }))
)

(define-read-only (get-department-budget-status (dept-id uint))
  (match (map-get? government-departments { dept-id: dept-id })
    dept-info (ok {
      total-budget: (get total-budget dept-info),
      spent-amount: (get spent-amount dept-info),
      remaining: (- (get total-budget dept-info) (get spent-amount dept-info)),
      utilization-rate: (if (> (get total-budget dept-info) u0)
                         (/ (* (get spent-amount dept-info) u100) (get total-budget dept-info))
                         u0)
    })
    ERR_INVALID_DEPARTMENT
  )
)

;; Administrative functions
(define-public (add-treasury-funds (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (var-set total-treasury (+ (var-get total-treasury) amount))
    (ok amount)
  )
)

(define-public (authorize-official (official principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-officials official true)
    (ok true)
  )
)

(define-public (revoke-authorization (official principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete authorized-officials official)
    (ok true)
  )
)

;; Department management functions
(define-public (create-department (name (string-ascii 50)) (head principal) (budget uint))
  (let ((dept-id (var-get next-dept-id)))
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> budget u0) ERR_INVALID_AMOUNT)
    (asserts! (<= budget (- (var-get total-treasury) (var-get total-allocated))) ERR_INSUFFICIENT_FUNDS)
    
    (map-set government-departments 
      { dept-id: dept-id }
      {
        name: name,
        head: head,
        total-budget: budget,
        spent-amount: u0,
        active: true
      }
    )
    
    (map-set department-managers { dept-id: dept-id, manager: head } true)
    (var-set next-dept-id (+ dept-id u1))
    (var-set total-allocated (+ (var-get total-allocated) budget))
    
    (ok dept-id)
  )
)

(define-public (update-department-budget (dept-id uint) (new-budget uint))
  (let ((dept-info (unwrap! (map-get? government-departments { dept-id: dept-id }) ERR_INVALID_DEPARTMENT)))
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> new-budget (get spent-amount dept-info)) ERR_INVALID_AMOUNT)
    
    (let ((budget-diff (if (> new-budget (get total-budget dept-info))
                         (- new-budget (get total-budget dept-info))
                         u0)))
      (asserts! (<= budget-diff (- (var-get total-treasury) (var-get total-allocated))) ERR_INSUFFICIENT_FUNDS)
      
      (map-set government-departments 
        { dept-id: dept-id }
        (merge dept-info { total-budget: new-budget })
      )
      
      (var-set total-allocated (+ (- (var-get total-allocated) (get total-budget dept-info)) new-budget))
      (ok true)
    )
  )
)

;; Project management functions
(define-public (create-project 
    (name (string-ascii 100)) 
    (dept-id uint) 
    (budget uint) 
    (manager principal) 
    (duration uint) 
    (description (string-ascii 200)))
  (let ((project-id (var-get next-project-id))
        (dept-info (unwrap! (map-get? government-departments { dept-id: dept-id }) ERR_INVALID_DEPARTMENT)))
    
    (asserts! (or (is-authorized tx-sender) 
                  (is-department-manager dept-id tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (> budget u0) ERR_INVALID_AMOUNT)
    (asserts! (<= (+ budget (get spent-amount dept-info)) (get total-budget dept-info)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set projects 
      { project-id: project-id }
      {
        name: name,
        department-id: dept-id,
        budget: budget,
        spent: u0,
        status: "active",
        manager: manager,
        start-block: block-height,
        end-block: (+ block-height duration),
        description: description
      }
    )
    
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (update-project-status (project-id uint) (status (string-ascii 20)))
  (let ((project-info (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND)))
    (asserts! (or (is-authorized tx-sender) 
                  (is-eq tx-sender (get manager project-info))) ERR_UNAUTHORIZED)
    
    (map-set projects 
      { project-id: project-id }
      (merge project-info { status: status })
    )
    
    (ok true)
  )
)

;; Expenditure functions
(define-public (create-expenditure 
    (project-id uint) 
    (amount uint) 
    (recipient principal) 
    (purpose (string-ascii 150)))
  (let ((tx-id (var-get next-tx-id))
        (project-info (unwrap! (map-get? projects { project-id: project-id }) ERR_PROJECT_NOT_FOUND))
        (dept-info (unwrap! (map-get? government-departments { dept-id: (get department-id project-info) }) ERR_INVALID_DEPARTMENT)))
    
    (asserts! (or (is-authorized tx-sender) 
                  (is-eq tx-sender (get manager project-info))) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= (+ amount (get spent project-info)) (get budget project-info)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set expenditures 
      { tx-id: tx-id }
      {
        project-id: project-id,
        amount: amount,
        recipient: recipient,
        purpose: purpose,
        timestamp: block-height,
        approved-by: tx-sender,
        status: "pending",
        receipt-hash: none
      }
    )
    
    (var-set next-tx-id (+ tx-id u1))
    (ok tx-id)
  )
)

(define-public (approve-expenditure (tx-id uint))
  (let ((expenditure-info (unwrap! (map-get? expenditures { tx-id: tx-id }) ERR_TRANSACTION_NOT_FOUND))
        (project-info (unwrap! (map-get? projects { project-id: (get project-id expenditure-info) }) ERR_PROJECT_NOT_FOUND))
        (dept-info (unwrap! (map-get? government-departments { dept-id: (get department-id project-info) }) ERR_INVALID_DEPARTMENT)))
    
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status expenditure-info) "pending") ERR_INVALID_STATUS)
    
    ;; Update expenditure status
    (map-set expenditures 
      { tx-id: tx-id }
      (merge expenditure-info { status: "approved" })
    )
    
    ;; Update project spent amount
    (map-set projects 
      { project-id: (get project-id expenditure-info) }
      (merge project-info { spent: (+ (get spent project-info) (get amount expenditure-info)) })
    )
    
    ;; Update department spent amount
    (map-set government-departments 
      { dept-id: (get department-id project-info) }
      (merge dept-info { spent-amount: (+ (get spent-amount dept-info) (get amount expenditure-info)) })
    )
    
    ;; Update total spent
    (var-set total-spent (+ (var-get total-spent) (get amount expenditure-info)))
    
    (ok true)
  )
)

(define-public (add-receipt-hash (tx-id uint) (receipt-hash (buff 32)))
  (let ((expenditure-info (unwrap! (map-get? expenditures { tx-id: tx-id }) ERR_TRANSACTION_NOT_FOUND)))
    (asserts! (or (is-authorized tx-sender) 
                  (is-eq tx-sender (get recipient expenditure-info))) ERR_UNAUTHORIZED)
    
    (map-set expenditures 
      { tx-id: tx-id }
      (merge expenditure-info { receipt-hash: (some receipt-hash) })
    )
    
    (ok true)
  )
)

;; Budget allocation functions
(define-public (allocate-budget (dept-id uint) (fiscal-year uint) (amount uint))
  (let ((allocation-id (var-get next-allocation-id)))
    (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (- (var-get total-treasury) (var-get total-allocated))) ERR_INSUFFICIENT_FUNDS)
    
    (map-set budget-allocations 
      { allocation-id: allocation-id }
      {
        department-id: dept-id,
        fiscal-year: fiscal-year,
        allocated-amount: amount,
        allocation-date: block-height,
        allocated-by: tx-sender
      }
    )
    
    (var-set next-allocation-id (+ allocation-id u1))
    (var-set total-allocated (+ (var-get total-allocated) amount))
    
    (ok allocation-id)
  )
)