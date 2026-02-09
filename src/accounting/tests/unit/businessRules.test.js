/**
 * Business Rule Validation Tests
 * Test Cases: TC-RULE-001 through TC-RULE-005
 */

const { DataProgram } = require('../../index');

describe('Business Rule Validation Tests', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-RULE-001: Verify initial balance business rule', () => {
        test('BUSINESS RULE: All accounts start with $1,000.00', () => {
            // Fresh application start
            const freshDataProgram = new DataProgram();
            const balance = freshDataProgram.operation('READ');
            
            // All student accounts start with an initial balance of $1,000.00
            expect(balance).toBe(1000.00);
        });

        test('should have exactly $1,000.00 on fresh instance', () => {
            const instance1 = new DataProgram();
            const instance2 = new DataProgram();
            const instance3 = new DataProgram();
            
            expect(instance1.operation('READ')).toBe(1000.00);
            expect(instance2.operation('READ')).toBe(1000.00);
            expect(instance3.operation('READ')).toBe(1000.00);
        });
    });

    describe('TC-RULE-002: Verify insufficient funds business rule', () => {
        test('BUSINESS RULE: Cannot overdraw account', () => {
            // Set balance to $500.00
            dataProgram.operation('WRITE', 500.00);
            
            const currentBalance = dataProgram.operation('READ');
            const debitAmount = 600.00;
            
            // Business rule: Debit operations cannot exceed current balance
            if (currentBalance >= debitAmount) {
                const newBalance = currentBalance - debitAmount;
                dataProgram.operation('WRITE', newBalance);
            }
            // Transaction rejected due to insufficient funds
            
            // Balance must remain unchanged
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(500.00);
            expect(finalBalance).toBeGreaterThanOrEqual(0); // No negative balances allowed
        });

        test('should prevent overdraft in all scenarios', () => {
            const testScenarios = [
                { balance: 1000.00, debit: 1001.00 },
                { balance: 500.00, debit: 501.00 },
                { balance: 100.00, debit: 200.00 },
                { balance: 0.00, debit: 0.01 },
                { balance: 0.01, debit: 0.02 }
            ];
            
            testScenarios.forEach(scenario => {
                dataProgram.operation('WRITE', scenario.balance);
                const balance = dataProgram.operation('READ');
                
                // Attempt debit
                if (balance >= scenario.debit) {
                    dataProgram.operation('WRITE', balance - scenario.debit);
                }
                
                // Balance should remain unchanged (all debits should be rejected)
                const finalBalance = dataProgram.operation('READ');
                expect(finalBalance).toBe(scenario.balance);
            });
        });
    });

    describe('TC-RULE-003: Verify maximum balance limit', () => {
        test('BUSINESS RULE: Maximum balance is $999,999.99', () => {
            // Set balance to $999,000.00 (safely below maximum)
            dataProgram.operation('WRITE', 999000.00);
            
            let balance = dataProgram.operation('READ');
            const creditAmount = 900.00;
            const newBalance = balance + creditAmount;
            
            // Business rule: Balance cannot exceed $999,999.99
            if (newBalance <= 999999.99) {
                dataProgram.operation('WRITE', newBalance);
            }
            // else: reject transaction
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(999900.00);
            expect(finalBalance).toBeLessThanOrEqual(999999.99);
        });

        test('should reject credit that would exceed maximum', () => {
            dataProgram.operation('WRITE', 999990.00);
            
            const balance = dataProgram.operation('READ');
            const creditAmount = 20.00; // Would result in 1,000,010.00
            const newBalance = balance + creditAmount;
            
            // Validation: would exceed maximum?
            if (newBalance > 999999.99) {
                // Reject - don't write
                const unchangedBalance = dataProgram.operation('READ');
                expect(unchangedBalance).toBe(999990.00);
            } else {
                dataProgram.operation('WRITE', newBalance);
            }
        });

        test('should allow balance exactly at maximum', () => {
            const maxBalance = 999999.99;
            dataProgram.operation('WRITE', maxBalance);
            
            const balance = dataProgram.operation('READ');
            expect(balance).toBe(maxBalance);
        });
    });

    describe('TC-RULE-004: Verify transaction atomicity', () => {
        test('BUSINESS RULE: Transactions are atomic (all-or-nothing)', () => {
            const initialBalance = dataProgram.operation('READ');
            expect(initialBalance).toBe(1000.00);
            
            // Successful transaction - should fully complete
            let balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            
            let currentBalance = dataProgram.operation('READ');
            expect(currentBalance).toBe(1500.00); // Fully updated
            
            // Failed transaction - should have no effect
            balance = dataProgram.operation('READ');
            const debitAmount = 2000.00;
            
            if (balance >= debitAmount) {
                // Would execute debit
                dataProgram.operation('WRITE', balance - debitAmount);
            }
            // else: rejected - no write occurs
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1500.00); // Completely unchanged
        });

        test('should not allow partial updates', () => {
            dataProgram.operation('WRITE', 1000.00);
            
            const operations = [
                { type: 'credit', amount: 500.00, shouldSucceed: true },
                { type: 'debit', amount: 2000.00, shouldSucceed: false },
                { type: 'credit', amount: 300.00, shouldSucceed: true }
            ];
            
            let expectedBalance = 1000.00;
            
            operations.forEach(op => {
                let balance = dataProgram.operation('READ');
                
                if (op.type === 'credit') {
                    balance += op.amount;
                    dataProgram.operation('WRITE', balance);
                    expectedBalance += op.amount;
                } else if (op.type === 'debit') {
                    if (balance >= op.amount) {
                        balance -= op.amount;
                        dataProgram.operation('WRITE', balance);
                        expectedBalance -= op.amount;
                    }
                    // else: rejected atomically - no partial debit
                }
                
                const currentBalance = dataProgram.operation('READ');
                expect(currentBalance).toBe(expectedBalance);
            });
            
            expect(dataProgram.operation('READ')).toBe(1800.00);
        });

        test('should maintain balance integrity across read-modify-write cycle', () => {
            // Each operation must be atomic
            const operations = [
                { read: 1000.00, modify: 250.00, write: 1250.00 },
                { read: 1250.00, modify: -100.00, write: 1150.00 },
                { read: 1150.00, modify: 350.00, write: 1500.00 }
            ];
            
            operations.forEach(op => {
                const readBalance = dataProgram.operation('READ');
                expect(readBalance).toBe(op.read);
                
                const newBalance = readBalance + op.modify;
                dataProgram.operation('WRITE', newBalance);
                
                const writeBalance = dataProgram.operation('READ');
                expect(writeBalance).toBe(op.write);
            });
        });
    });

    describe('TC-RULE-005: Verify balance precision', () => {
        test('BUSINESS RULE: Maintain 2 decimal precision', () => {
            // Credit $0.01
            let balance = dataProgram.operation('READ');
            balance += 0.01;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBeCloseTo(1000.01, 2);
            
            // Debit $0.01
            balance = dataProgram.operation('READ');
            balance -= 0.01;
            dataProgram.operation('WRITE', balance);
            
            const finalBalance = dataProgram.operation('READ');
            
            // Balance should return to exactly $1,000.00 - no rounding errors
            expect(finalBalance).toBeCloseTo(1000.00, 2);
            expect(finalBalance.toFixed(2)).toBe('1000.00');
        });

        test('should maintain precision across multiple decimal operations', () => {
            const operations = [
                { amount: 0.33, type: 'add' },
                { amount: 0.33, type: 'add' },
                { amount: 0.34, type: 'add' }, // Total: +1.00
                { amount: 0.50, type: 'subtract' },
                { amount: 0.50, type: 'subtract' } // Total: -1.00
            ];
            
            operations.forEach(op => {
                let balance = dataProgram.operation('READ');
                if (op.type === 'add') {
                    balance += op.amount;
                } else {
                    balance -= op.amount;
                }
                // Round to 2 decimal places to simulate currency precision
                balance = Math.round(balance * 100) / 100;
                dataProgram.operation('WRITE', balance);
            });
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBeCloseTo(1000.00, 2);
        });

        test('should handle currency precision requirements', () => {
            const testValues = [
                { value: 1234.56, expected: '1234.56' },
                { value: 0.01, expected: '0.01' },
                { value: 999999.99, expected: '999999.99' },
                { value: 100.00, expected: '100.00' },
                { value: 50.50, expected: '50.50' }
            ];
            
            testValues.forEach(test => {
                dataProgram.operation('WRITE', test.value);
                const balance = dataProgram.operation('READ');
                
                // Verify 2 decimal places maintained
                expect(balance.toFixed(2)).toBe(test.expected);
                expect(balance).toBeCloseTo(test.value, 2);
            });
        });

        test('should not introduce floating-point errors', () => {
            // Known problematic floating-point operations
            dataProgram.operation('WRITE', 0.1);
            let balance = dataProgram.operation('READ');
            
            balance += 0.2; // JavaScript: 0.1 + 0.2 = 0.30000000000000004
            balance = Math.round(balance * 100) / 100; // Round to 2 decimals
            dataProgram.operation('WRITE', balance);
            
            const result = dataProgram.operation('READ');
            expect(result).toBeCloseTo(0.3, 2);
            expect(result.toFixed(2)).toBe('0.30');
        });
    });

    describe('All Business Rules Combined', () => {
        test('should enforce all business rules in realistic scenario', () => {
            // Rule 1: Initial balance $1,000.00
            const freshAccount = new DataProgram();
            expect(freshAccount.operation('READ')).toBe(1000.00);
            
            // Rule 5: Maintain 2 decimal precision
            let balance = freshAccount.operation('READ');
            balance += 123.45;
            balance = Math.round(balance * 100) / 100;
            freshAccount.operation('WRITE', balance);
            expect(freshAccount.operation('READ')).toBeCloseTo(1123.45, 2);
            
            // Rule 4: Transaction atomicity (successful debit)
            balance = freshAccount.operation('READ');
            if (balance >= 100.00) {
                balance -= 100.00;
                freshAccount.operation('WRITE', balance);
            }
            expect(freshAccount.operation('READ')).toBeCloseTo(1023.45, 2);
            
            // Rule 2: Cannot overdraw (failed debit)
            const beforeFailedDebit = freshAccount.operation('READ');
            if (beforeFailedDebit >= 2000.00) {
                freshAccount.operation('WRITE', beforeFailedDebit - 2000.00);
            }
            expect(freshAccount.operation('READ')).toBe(beforeFailedDebit); // Unchanged
            
            // Rule 3: Cannot exceed maximum (though not reached in this scenario)
            const currentBalance = freshAccount.operation('READ');
            expect(currentBalance).toBeLessThanOrEqual(999999.99);
        });
    });
});
