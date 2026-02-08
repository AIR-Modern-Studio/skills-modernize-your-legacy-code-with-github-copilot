# Test Plan: Account Management System

## Document Information

**Application Name**: Account Management System (COBOL Legacy Application)  
**Version**: 1.0  
**Test Plan Created**: February 8, 2026  
**Purpose**: Validate business logic before migration to Node.js  
**Target Platform**: Node.js (Future Implementation)

---

## Test Objectives

1. Verify all account management operations function correctly
2. Validate business rules enforcement (e.g., insufficient funds)
3. Ensure data integrity across operations
4. Confirm user interface behavior and error handling
5. Document expected behavior for Node.js migration

---

## Test Scope

### In Scope

- View balance functionality
- Credit account operations
- Debit account operations
- Insufficient funds validation
- Menu navigation and user input handling
- Initial balance configuration
- Exit functionality

### Out of Scope

- Multi-user concurrent access
- Database persistence (future enhancement)
- Authentication/authorization (future enhancement)
- Transaction history (future enhancement)

---

## Test Cases

### 1. System Initialization Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-INIT-001 | Verify initial balance is set correctly | Application is started for the first time | 1. Start application<br/>2. Select option 1 (View Balance) | System displays balance of $1,000.00 | | | Initial balance defined in DataProgram |
| TC-INIT-002 | Verify main menu displays correctly | Application is started | 1. Start application | Menu displays with options:<br/>1. View Balance<br/>2. Credit Account<br/>3. Debit Account<br/>4. Exit | | | Menu should be user-friendly and clear |

---

### 2. View Balance Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-VIEW-001 | View initial balance | Application started with default balance | 1. Select option 1 (View Balance) | System displays: "Current balance: 001000.00" | | | Format includes leading zeros |
| TC-VIEW-002 | View balance after credit operation | Balance has been credited with $500.00 | 1. Credit $500.00<br/>2. Select option 1 (View Balance) | System displays: "Current balance: 001500.00" | | | Balance should reflect previous credit |
| TC-VIEW-003 | View balance after debit operation | Balance has been debited with $300.00 | 1. Debit $300.00<br/>2. Select option 1 (View Balance) | System displays updated balance (e.g., "Current balance: 000700.00") | | | Balance should reflect previous debit |
| TC-VIEW-004 | View balance multiple times | Application is running | 1. Select option 1 (View Balance)<br/>2. Select option 1 again<br/>3. Repeat multiple times | Same balance displayed each time (no changes without transactions) | | | Read operation should not modify data |

---

### 3. Credit Account Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-CREDIT-001 | Credit account with valid amount | Current balance is $1,000.00 | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 500.00 | System displays: "Amount credited. New balance: 001500.00"<br/>Balance increases to $1,500.00 | | | Basic credit operation |
| TC-CREDIT-002 | Credit account with small amount | Current balance is $1,000.00 | 1. Select option 2<br/>2. Enter amount: 0.01 | System displays: "Amount credited. New balance: 001000.01"<br/>Balance increases by $0.01 | | | Test minimum transaction |
| TC-CREDIT-003 | Credit account with large amount | Current balance is $1,000.00 | 1. Select option 2<br/>2. Enter amount: 50000.00 | System displays: "Amount credited. New balance: 051000.00"<br/>Balance increases to $51,000.00 | | | Test large transaction |
| TC-CREDIT-004 | Credit account multiple times | Current balance is $1,000.00 | 1. Credit $200.00<br/>2. Credit $300.00<br/>3. Credit $100.00 | Final balance is $1,600.00<br/>Each operation shows correct cumulative balance | | | Test sequential credits |
| TC-CREDIT-005 | Credit account with decimal precision | Current balance is $1,000.00 | 1. Select option 2<br/>2. Enter amount: 123.45 | System displays: "Amount credited. New balance: 001123.45"<br/>Decimal precision maintained | | | Test decimal handling |
| TC-CREDIT-006 | Credit account approaching maximum balance | Current balance is $995,000.00 | 1. Select option 2<br/>2. Enter amount: 4000.00 | System displays: "Amount credited. New balance: 999000.00"<br/>Balance updates correctly | | | Test near-maximum values (max is 999999.99) |
| TC-CREDIT-007 | Credit account with zero amount | Current balance is $1,000.00 | 1. Select option 2<br/>2. Enter amount: 0.00 | System accepts and balance remains $1,000.00 OR system rejects invalid input | | | Edge case - may need validation |

---

### 4. Debit Account Tests (Sufficient Funds)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DEBIT-001 | Debit account with valid amount (sufficient funds) | Current balance is $1,000.00 | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 300.00 | System displays: "Amount debited. New balance: 000700.00"<br/>Balance decreases to $700.00 | | | Basic debit operation |
| TC-DEBIT-002 | Debit account with small amount | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 0.01 | System displays: "Amount debited. New balance: 000999.99"<br/>Balance decreases by $0.01 | | | Test minimum transaction |
| TC-DEBIT-003 | Debit exact balance amount | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 1000.00 | System displays: "Amount debited. New balance: 000000.00"<br/>Balance becomes $0.00 | | | Test boundary - complete withdrawal |
| TC-DEBIT-004 | Debit account multiple times | Current balance is $1,000.00 | 1. Debit $200.00<br/>2. Debit $150.00<br/>3. Debit $100.00 | Final balance is $550.00<br/>Each operation shows correct remaining balance | | | Test sequential debits |
| TC-DEBIT-005 | Debit account with decimal precision | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 123.45 | System displays: "Amount debited. New balance: 000876.55"<br/>Decimal precision maintained | | | Test decimal handling |
| TC-DEBIT-006 | Debit leaving small remaining balance | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 999.99 | System displays: "Amount debited. New balance: 000000.01"<br/>Balance is $0.01 | | | Test near-zero balance |

---

### 5. Debit Account Tests (Insufficient Funds)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DEBIT-INS-001 | Debit amount exceeds balance | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 1500.00 | System displays: "Insufficient funds for this debit."<br/>Balance remains $1,000.00 (unchanged) | | | **Critical business rule** |
| TC-DEBIT-INS-002 | Debit amount slightly exceeds balance | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 1000.01 | System displays: "Insufficient funds for this debit."<br/>Balance remains $1,000.00 | | | Test boundary - exceeds by $0.01 |
| TC-DEBIT-INS-003 | Debit from zero balance | Current balance is $0.00 | 1. Select option 3<br/>2. Enter amount: 0.01 | System displays: "Insufficient funds for this debit."<br/>Balance remains $0.00 | | | Cannot debit from zero balance |
| TC-DEBIT-INS-004 | Debit with very large amount | Current balance is $1,000.00 | 1. Select option 3<br/>2. Enter amount: 999999.99 | System displays: "Insufficient funds for this debit."<br/>Balance remains $1,000.00 | | | Test with maximum possible amount |
| TC-DEBIT-INS-005 | Verify balance unchanged after insufficient funds | Current balance is $500.00 | 1. Attempt to debit $1,000.00 (fails)<br/>2. Check balance | Balance still shows $500.00<br/>No partial debit occurred | | | Ensure transaction atomicity |

---

### 6. Mixed Operation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-MIX-001 | Credit then debit sequence | Starting balance is $1,000.00 | 1. Credit $500.00<br/>2. Debit $300.00<br/>3. View balance | Final balance is $1,200.00<br/>Each step shows correct intermediate balance | | | Test operation sequence |
| TC-MIX-002 | Debit then credit sequence | Starting balance is $1,000.00 | 1. Debit $400.00<br/>2. Credit $600.00<br/>3. View balance | Final balance is $1,200.00<br/>Each step shows correct intermediate balance | | | Test reverse sequence |
| TC-MIX-003 | Multiple mixed operations | Starting balance is $1,000.00 | 1. Credit $500.00<br/>2. Debit $200.00<br/>3. Credit $300.00<br/>4. Debit $100.00<br/>5. View balance | Final balance is $1,500.00<br/>All operations succeed | | | Complex transaction sequence |
| TC-MIX-004 | Failed debit doesn't affect subsequent operations | Starting balance is $1,000.00 | 1. Attempt to debit $2,000.00 (fails)<br/>2. Credit $500.00<br/>3. Debit $300.00<br/>4. View balance | Balance is $1,200.00<br/>Failed operation doesn't corrupt state | | | Test error recovery |
| TC-MIX-005 | Alternating view and transaction operations | Starting balance is $1,000.00 | 1. View balance<br/>2. Credit $200.00<br/>3. View balance<br/>4. Debit $100.00<br/>5. View balance | Each view shows correct current balance:<br/>$1,000.00 → $1,200.00 → $1,100.00 | | | Test read operations don't interfere |

---

### 7. Menu Navigation and Input Validation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-MENU-001 | Select invalid menu option (too high) | Application is running | 1. Enter choice: 5 | System displays: "Invalid choice, please select 1-4."<br/>Menu redisplays | | | Input validation |
| TC-MENU-002 | Select invalid menu option (zero) | Application is running | 1. Enter choice: 0 | System displays: "Invalid choice, please select 1-4."<br/>Menu redisplays | | | Edge case validation |
| TC-MENU-003 | Navigate all menu options | Application is running | 1. Select option 1<br/>2. Return to menu<br/>3. Select option 2 (enter amount)<br/>4. Return to menu<br/>5. Select option 3 (enter amount)<br/>6. Return to menu | All options accessible and return to menu correctly | | | Test navigation flow |
| TC-MENU-004 | Exit application | Application is running | 1. Select option 4 (Exit) | System displays: "Exiting the program. Goodbye!"<br/>Application terminates cleanly | | | Clean exit |
| TC-MENU-005 | Menu redisplays after each operation | Application is running | 1. Perform any operation<br/>2. Observe behavior | Menu automatically redisplays after operation completion | | | User experience |

---

### 8. Data Integrity and Persistence Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-DATA-001 | Balance persists across operations within session | Application is running | 1. Credit $500.00<br/>2. Perform unrelated operation (view menu)<br/>3. View balance | Balance still shows $1,500.00 | | | In-memory persistence |
| TC-DATA-002 | Verify read operation doesn't modify balance | Current balance is $1,000.00 | 1. View balance 10 times<br/>2. Verify balance | Balance remains exactly $1,000.00 | | | Read operations are non-destructive |
| TC-DATA-003 | Balance format consistency | Various balance amounts | 1. Create different balances<br/>2. View each balance | All balances display in format: 9(6)V99<br/>(e.g., 001234.56) | | | Consistent formatting |
| TC-DATA-004 | Verify no balance corruption | Starting balance is $1,000.00 | 1. Perform 20 mixed operations<br/>2. Manually calculate expected balance<br/>3. Compare with displayed balance | Calculated balance matches displayed balance exactly | | | Mathematical accuracy |

---

### 9. Business Rule Validation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-RULE-001 | Verify initial balance business rule | Fresh application start | 1. Start application<br/>2. View balance immediately | Balance is exactly $1,000.00 | | | **Business Rule**: All accounts start with $1,000 |
| TC-RULE-002 | Verify insufficient funds business rule | Balance is $500.00 | 1. Attempt to debit $600.00 | Transaction rejected<br/>Error message displayed<br/>Balance unchanged | | | **Business Rule**: Cannot overdraw account |
| TC-RULE-003 | Verify maximum balance limit | Balance is $999,995.00 | 1. Attempt to credit $10.00 | System accepts OR rejects if exceeds limit<br/>Balance should not exceed $999,999.99 | | | **Business Rule**: Max balance is 999999.99 |
| TC-RULE-004 | Verify transaction atomicity | Current balance is $1,000.00 | 1. Perform operation that should update balance<br/>2. Verify balance changed or remained unchanged (no partial updates) | Balance either fully updated or remains unchanged<br/>No intermediate states visible | | | **Business Rule**: Transactions are atomic |
| TC-RULE-005 | Verify balance precision | Current balance is $1,000.00 | 1. Credit $0.01<br/>2. Debit $0.01<br/>3. Verify balance | Balance returns to exactly $1,000.00<br/>No rounding errors | | | **Business Rule**: Maintain 2 decimal precision |

---

### 10. Edge Cases and Boundary Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-EDGE-001 | Test with maximum valid balance | Current balance is $999,999.99 | 1. View balance<br/>2. Attempt to debit small amount | Balance displays correctly<br/>Debit operations work | | | Test upper boundary |
| TC-EDGE-002 | Test with minimum valid balance | Current balance is $0.00 | 1. View balance<br/>2. Attempt operations | Balance displays as 000000.00<br/>Credits work, debits rejected | | | Test lower boundary |
| TC-EDGE-003 | Test with precisely calculated balances | Starting balance is $1,000.00 | 1. Credit $333.33<br/>2. Debit $333.33<br/>3. Verify balance | Balance returns to $1,000.00<br/>No floating-point errors | | | Test decimal arithmetic |
| TC-EDGE-004 | Rapid sequential operations | Starting balance is $1,000.00 | 1. Perform 50 rapid operations (credit/debit alternating) | All operations complete successfully<br/>Final balance is mathematically correct | | | Stress test for single session |
| TC-EDGE-005 | Test balance after reducing to zero then crediting | Starting balance is $1,000.00 | 1. Debit $1,000.00 (balance = $0.00)<br/>2. Credit $500.00<br/>3. View balance | Balance is $500.00<br/>Operations work correctly from zero | | | Test zero-balance recovery |

---

## Test Execution Guidelines

### For Manual Testing

1. Review pre-conditions before starting each test
2. Execute test steps in the exact order specified
3. Record actual results in the "Actual Result" column
4. Mark status as "Pass" if actual matches expected, "Fail" otherwise
5. Document any deviations or issues in the Comments column

### For Automated Testing (Node.js Future Implementation)

1. Each test case should map to one or more unit/integration tests
2. Test case IDs should reference corresponding test file names
3. Pre-conditions should be implemented as test setup/fixtures
4. Expected results should be assertions in test code
5. Use test case descriptions as test function names

---

## Test Data Requirements

### Initial Test Data

- Default starting balance: $1,000.00
- Test amounts for credits: $0.01, $100.00, $500.00, $1,000.00, $50,000.00
- Test amounts for debits: $0.01, $100.00, $300.00, $1,000.00, $1,500.00

### Boundary Values

- Minimum balance: $0.00
- Maximum balance: $999,999.99
- Minimum transaction: $0.01
- Maximum transaction: $999,999.99

---

## Success Criteria

A test case is considered **PASSED** if:

- The actual result matches the expected result exactly
- Balance calculations are mathematically correct
- Error messages are displayed when appropriate
- Application remains stable and doesn't crash
- Data integrity is maintained throughout the operation

A test case is considered **FAILED** if:

- Actual result differs from expected result
- Balance calculations are incorrect
- Application crashes or hangs
- Data corruption occurs
- Business rules are violated

---

## Test Environment

### Current Environment (COBOL)

- Platform: Linux (Ubuntu 22.04.5 LTS)
- COBOL Compiler: GnuCOBOL (cobc)
- Executable: accountsystem

### Target Environment (Node.js)

- Platform: Linux/Windows/macOS
- Node.js Version: 18.x or higher
- Testing Framework: Jest/Mocha (TBD)
- Test Coverage Target: > 90%

---

## Risks and Assumptions

### Assumptions

1. COBOL application behavior is the "source of truth" for Node.js migration
2. All business rules documented are current and accurate
3. Maximum balance limit of $999,999.99 is enforced by data type
4. Single-user environment (no concurrent access)

### Risks

1. **Data Type Differences**: JavaScript Number type handles decimals differently than COBOL PIC 9(6)V99
2. **Rounding Errors**: Need to use decimal libraries (e.g., decimal.js) in Node.js
3. **Input Validation**: COBOL implicit validation may need explicit implementation in Node.js
4. **User Interface**: Terminal UI behavior may differ between COBOL and Node.js implementations

---

## Migration Validation Checklist

Before declaring Node.js migration complete, verify:

- [ ] All 60+ test cases pass in Node.js implementation
- [ ] Balance calculations match COBOL output exactly
- [ ] All business rules are enforced
- [ ] Error messages match COBOL behavior
- [ ] Insufficient funds validation works correctly
- [ ] Initial balance is $1,000.00
- [ ] Decimal precision is maintained (2 decimal places)
- [ ] Menu navigation works as expected
- [ ] Clean exit functionality
- [ ] No data corruption under any scenario

---

## Test Execution Summary Template

| Test Category | Total Tests | Passed | Failed | Blocked | Pass Rate |
|---------------|-------------|--------|--------|---------|-----------|
| System Initialization | 2 | | | | |
| View Balance | 4 | | | | |
| Credit Account | 7 | | | | |
| Debit Account (Sufficient Funds) | 6 | | | | |
| Debit Account (Insufficient Funds) | 5 | | | | |
| Mixed Operations | 5 | | | | |
| Menu Navigation | 5 | | | | |
| Data Integrity | 4 | | | | |
| Business Rules | 5 | | | | |
| Edge Cases | 5 | | | | |
| **TOTAL** | **48** | | | | |

---

## Test Sign-off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Test Lead | | | |
| Business Analyst | | | |
| Development Lead | | | |
| Product Owner | | | |

---

## Appendix A: Test Automation Mapping

### Recommended Test Structure for Node.js

```
tests/
├── unit/
│   ├── balance.test.js          # TC-VIEW-*, TC-DATA-*
│   ├── credit.test.js           # TC-CREDIT-*
│   ├── debit.test.js            # TC-DEBIT-*, TC-DEBIT-INS-*
│   ├── validation.test.js       # TC-MENU-*, input validation
│   └── businessRules.test.js    # TC-RULE-*
├── integration/
│   ├── operations.test.js       # TC-MIX-*
│   ├── dataFlow.test.js         # End-to-end operation flows
│   └── edgeCases.test.js        # TC-EDGE-*
└── e2e/
    └── userJourney.test.js      # Full user scenarios
```

### Key Testing Libraries for Node.js

- **Jest**: Test framework and assertions
- **decimal.js** or **big.js**: Precise decimal arithmetic
- **Supertest**: API testing (if building REST API)
- **Mock-stdin**: Testing terminal input (for CLI version)

---

## Appendix B: Business Logic Summary

### Core Business Rules (Must Preserve in Migration)

1. **Initial Balance**: $1,000.00
2. **Insufficient Funds**: Debit rejected if amount > balance
3. **Decimal Precision**: 2 decimal places, no rounding errors
4. **Balance Range**: $0.00 to $999,999.99
5. **Transaction Atomicity**: All-or-nothing operations
6. **Read-Only View**: Balance viewing doesn't modify state

### Critical Test Cases for Stakeholder Review

- TC-DEBIT-INS-001: Insufficient funds protection
- TC-RULE-001: Initial balance $1,000.00
- TC-RULE-002: Cannot overdraw account
- TC-EDGE-003: No floating-point arithmetic errors
- TC-MIX-004: Failed operations don't corrupt state

---

**Document Version**: 1.0  
**Last Updated**: February 8, 2026  
**Next Review Date**: Upon completion of Node.js migration
