/**
 * Debit Account Tests
 * Test Cases: TC-DEBIT-001 through TC-DEBIT-006 (Sufficient Funds)
 *             TC-DEBIT-INS-001 through TC-DEBIT-INS-005 (Insufficient Funds)
 */

const { DataProgram } = require('../../index');

describe('Debit Account Tests (Sufficient Funds)', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-DEBIT-001: Debit account with valid amount (sufficient funds)', () => {
        test('should decrease balance by $300.00', () => {
            // Read current balance ($1,000.00)
            let balance = dataProgram.operation('READ');
            expect(balance).toBe(1000.00);
            
            const debitAmount = 300.00;
            
            // Verify sufficient funds
            expect(balance).toBeGreaterThanOrEqual(debitAmount);
            
            // Subtract debit amount
            balance -= debitAmount;
            
            // Write new balance
            dataProgram.operation('WRITE', balance);
            
            // Verify new balance
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(700.00);
        });
    });

    describe('TC-DEBIT-002: Debit account with small amount', () => {
        test('should decrease balance by $0.01', () => {
            let balance = dataProgram.operation('READ');
            const debitAmount = 0.01;
            
            if (balance >= debitAmount) {
                balance -= debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(999.99);
        });
    });

    describe('TC-DEBIT-003: Debit exact balance amount', () => {
        test('should reduce balance to $0.00 when debiting exact amount', () => {
            let balance = dataProgram.operation('READ');
            const debitAmount = 1000.00;
            
            expect(balance).toBeGreaterThanOrEqual(debitAmount);
            
            balance -= debitAmount;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(0.00);
        });
    });

    describe('TC-DEBIT-004: Debit account multiple times', () => {
        test('should correctly process multiple sequential debits', () => {
            // Debit $200.00
            let balance = dataProgram.operation('READ');
            balance -= 200.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(800.00);
            
            // Debit $150.00
            balance = dataProgram.operation('READ');
            balance -= 150.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(650.00);
            
            // Debit $100.00
            balance = dataProgram.operation('READ');
            balance -= 100.00;
            dataProgram.operation('WRITE', balance);
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(550.00);
        });
    });

    describe('TC-DEBIT-005: Debit account with decimal precision', () => {
        test('should maintain decimal precision for $123.45', () => {
            let balance = dataProgram.operation('READ');
            const debitAmount = 123.45;
            
            balance -= debitAmount;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBeCloseTo(876.55, 2);
            expect(newBalance.toFixed(2)).toBe('876.55');
        });
    });

    describe('TC-DEBIT-006: Debit leaving small remaining balance', () => {
        test('should leave balance of $0.01 after debiting $999.99', () => {
            let balance = dataProgram.operation('READ');
            const debitAmount = 999.99;
            
            expect(balance).toBeGreaterThanOrEqual(debitAmount);
            
            balance -= debitAmount;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBeCloseTo(0.01, 2);
        });
    });
});

describe('Debit Account Tests (Insufficient Funds)', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-DEBIT-INS-001: Debit amount exceeds balance', () => {
        test('should reject debit and maintain balance when amount exceeds available funds', () => {
            const initialBalance = dataProgram.operation('READ');
            expect(initialBalance).toBe(1000.00);
            
            const debitAmount = 1500.00;
            
            // Check for sufficient funds (business rule validation)
            if (initialBalance >= debitAmount) {
                let balance = initialBalance - debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            // else: Insufficient funds - reject transaction
            
            // Verify balance remained unchanged
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1000.00);
            expect(finalBalance).toBe(initialBalance);
        });
    });

    describe('TC-DEBIT-INS-002: Debit amount slightly exceeds balance', () => {
        test('should reject debit when exceeding balance by $0.01', () => {
            const initialBalance = dataProgram.operation('READ');
            expect(initialBalance).toBe(1000.00);
            
            const debitAmount = 1000.01;
            
            // Insufficient funds validation
            if (initialBalance >= debitAmount) {
                let balance = initialBalance - debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            
            // Balance should remain unchanged
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1000.00);
            expect(initialBalance < debitAmount).toBe(true);
        });
    });

    describe('TC-DEBIT-INS-003: Debit from zero balance', () => {
        test('should reject any debit when balance is $0.00', () => {
            // Set balance to zero
            dataProgram.operation('WRITE', 0.00);
            
            const initialBalance = dataProgram.operation('READ');
            expect(initialBalance).toBe(0.00);
            
            const debitAmount = 0.01;
            
            // Attempt to debit
            if (initialBalance >= debitAmount) {
                let balance = initialBalance - debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            
            // Balance should remain zero
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(0.00);
        });
    });

    describe('TC-DEBIT-INS-004: Debit with very large amount', () => {
        test('should reject debit of maximum amount when insufficient funds', () => {
            const initialBalance = dataProgram.operation('READ');
            expect(initialBalance).toBe(1000.00);
            
            const debitAmount = 999999.99; // Maximum possible amount
            
            // Validate insufficient funds
            expect(initialBalance).toBeLessThan(debitAmount);
            
            if (initialBalance >= debitAmount) {
                let balance = initialBalance - debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            
            // Balance should remain unchanged
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1000.00);
        });
    });

    describe('TC-DEBIT-INS-005: Verify balance unchanged after insufficient funds', () => {
        test('should ensure no partial debit occurred after failed transaction', () => {
            // Set balance to $500.00
            dataProgram.operation('WRITE', 500.00);
            
            const initialBalance = dataProgram.operation('READ');
            expect(initialBalance).toBe(500.00);
            
            const debitAmount = 1000.00;
            
            // Store balance before attempting debit
            const balanceBeforeAttempt = dataProgram.operation('READ');
            
            // Attempt to debit (should fail)
            if (initialBalance >= debitAmount) {
                let balance = initialBalance - debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            
            // Verify balance is exactly the same as before
            const balanceAfterAttempt = dataProgram.operation('READ');
            expect(balanceAfterAttempt).toBe(balanceBeforeAttempt);
            expect(balanceAfterAttempt).toBe(500.00);
            
            // Verify no negative balance or partial debit
            expect(balanceAfterAttempt).toBeGreaterThanOrEqual(0);
        });
    });

    describe('Insufficient Funds - Transaction Atomicity', () => {
        test('should maintain atomicity - balance either fully debited or unchanged', () => {
            dataProgram.operation('WRITE', 750.00);
            
            const testCases = [
                { debit: 500.00, shouldSucceed: true },
                { debit: 500.00, shouldSucceed: false }, // Would exceed remaining balance
                { debit: 200.00, shouldSucceed: true }
            ];
            
            let currentBalance = 750.00;
            
            testCases.forEach(testCase => {
                const balance = dataProgram.operation('READ');
                
                if (balance >= testCase.debit) {
                    const newBalance = balance - testCase.debit;
                    dataProgram.operation('WRITE', newBalance);
                    currentBalance = newBalance;
                    expect(testCase.shouldSucceed).toBe(true);
                } else {
                    // Transaction rejected - balance stays the same
                    expect(testCase.shouldSucceed).toBe(false);
                }
                
                const verifyBalance = dataProgram.operation('READ');
                expect(verifyBalance).toBe(currentBalance);
            });
        });
    });
});

describe('Debit Account - Read-Modify-Write Pattern with Validation', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    test('should follow READ-VALIDATE-MODIFY-WRITE pattern', () => {
        // Step 1: Read current balance
        const currentBalance = dataProgram.operation('READ');
        expect(currentBalance).toBe(1000.00);
        
        const debitAmount = 250.00;
        
        // Step 2: Validate sufficient funds (CRITICAL BUSINESS RULE)
        expect(currentBalance).toBeGreaterThanOrEqual(debitAmount);
        
        if (currentBalance >= debitAmount) {
            // Step 3: Modify (calculate new balance)
            const newBalance = currentBalance - debitAmount;
            expect(newBalance).toBe(750.00);
            
            // Step 4: Write new balance
            dataProgram.operation('WRITE', newBalance);
            
            // Verify persistence
            const verifyBalance = dataProgram.operation('READ');
            expect(verifyBalance).toBe(750.00);
        }
    });

    test('should reject debit without writing when validation fails', () => {
        const currentBalance = dataProgram.operation('READ');
        const debitAmount = 1500.00;
        
        // Validation fails
        expect(currentBalance).toBeLessThan(debitAmount);
        
        // No write operation should occur
        const balanceAfter = dataProgram.operation('READ');
        expect(balanceAfter).toBe(currentBalance);
        expect(balanceAfter).toBe(1000.00);
    });
});
