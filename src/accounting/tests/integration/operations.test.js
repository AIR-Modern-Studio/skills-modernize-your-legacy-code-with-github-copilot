/**
 * Mixed Operations Integration Tests
 * Test Cases: TC-MIX-001 through TC-MIX-005
 */

const { DataProgram } = require('../../index');

describe('Mixed Operation Tests', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-MIX-001: Credit then debit sequence', () => {
        test('should correctly process credit followed by debit', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Step 1: Credit $500.00
            let balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            
            const afterCredit = dataProgram.operation('READ');
            expect(afterCredit).toBe(1500.00);
            
            // Step 2: Debit $300.00
            balance = dataProgram.operation('READ');
            if (balance >= 300.00) {
                balance -= 300.00;
                dataProgram.operation('WRITE', balance);
            }
            
            const afterDebit = dataProgram.operation('READ');
            expect(afterDebit).toBe(1200.00);
            
            // Step 3: View balance (final verification)
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1200.00);
        });
    });

    describe('TC-MIX-002: Debit then credit sequence', () => {
        test('should correctly process debit followed by credit', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Step 1: Debit $400.00
            let balance = dataProgram.operation('READ');
            if (balance >= 400.00) {
                balance -= 400.00;
                dataProgram.operation('WRITE', balance);
            }
            
            const afterDebit = dataProgram.operation('READ');
            expect(afterDebit).toBe(600.00);
            
            // Step 2: Credit $600.00
            balance = dataProgram.operation('READ');
            balance += 600.00;
            dataProgram.operation('WRITE', balance);
            
            const afterCredit = dataProgram.operation('READ');
            expect(afterCredit).toBe(1200.00);
            
            // Step 3: View balance (final verification)
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1200.00);
        });
    });

    describe('TC-MIX-003: Multiple mixed operations', () => {
        test('should correctly process complex sequence of operations', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Step 1: Credit $500.00
            let balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(1500.00);
            
            // Step 2: Debit $200.00
            balance = dataProgram.operation('READ');
            if (balance >= 200.00) {
                balance -= 200.00;
                dataProgram.operation('WRITE', balance);
            }
            expect(dataProgram.operation('READ')).toBe(1300.00);
            
            // Step 3: Credit $300.00
            balance = dataProgram.operation('READ');
            balance += 300.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(1600.00);
            
            // Step 4: Debit $100.00
            balance = dataProgram.operation('READ');
            if (balance >= 100.00) {
                balance -= 100.00;
                dataProgram.operation('WRITE', balance);
            }
            expect(dataProgram.operation('READ')).toBe(1500.00);
            
            // Step 5: View balance (final verification)
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1500.00);
        });
    });

    describe('TC-MIX-004: Failed debit doesn\'t affect subsequent operations', () => {
        test('should maintain state integrity after failed transaction', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Step 1: Attempt to debit $2,000.00 (should fail)
            let balance = dataProgram.operation('READ');
            const debitAmount = 2000.00;
            
            if (balance >= debitAmount) {
                balance -= debitAmount;
                dataProgram.operation('WRITE', balance);
            }
            // Transaction rejected - balance should remain $1,000.00
            
            const afterFailedDebit = dataProgram.operation('READ');
            expect(afterFailedDebit).toBe(1000.00);
            
            // Step 2: Credit $500.00 (should succeed)
            balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            
            const afterCredit = dataProgram.operation('READ');
            expect(afterCredit).toBe(1500.00);
            
            // Step 3: Debit $300.00 (should succeed)
            balance = dataProgram.operation('READ');
            if (balance >= 300.00) {
                balance -= 300.00;
                dataProgram.operation('WRITE', balance);
            }
            
            const afterDebit = dataProgram.operation('READ');
            expect(afterDebit).toBe(1200.00);
            
            // Step 4: View balance
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1200.00);
            
            // Verify failed operation didn't corrupt state
            expect(finalBalance).toBeGreaterThan(0);
        });
    });

    describe('TC-MIX-005: Alternating view and transaction operations', () => {
        test('should show correct balance after each operation', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Step 1: View balance
            let balance = dataProgram.operation('READ');
            expect(balance).toBe(1000.00);
            
            // Step 2: Credit $200.00
            balance = dataProgram.operation('READ');
            balance += 200.00;
            dataProgram.operation('WRITE', balance);
            
            // Step 3: View balance
            balance = dataProgram.operation('READ');
            expect(balance).toBe(1200.00);
            
            // Step 4: Debit $100.00
            balance = dataProgram.operation('READ');
            if (balance >= 100.00) {
                balance -= 100.00;
                dataProgram.operation('WRITE', balance);
            }
            
            // Step 5: View balance
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1100.00);
            
            // Verify each view showed correct current balance
            // $1,000.00 → $1,200.00 → $1,100.00
        });
    });

    describe('Complex Mixed Operation Scenarios', () => {
        test('should handle 10+ operations maintaining consistency', () => {
            let expectedBalance = 1000.00;
            
            const operations = [
                { type: 'credit', amount: 250.00 },   // 1250.00
                { type: 'debit', amount: 100.00 },    // 1150.00
                { type: 'credit', amount: 500.00 },   // 1650.00
                { type: 'debit', amount: 200.00 },    // 1450.00
                { type: 'credit', amount: 50.00 },    // 1500.00
                { type: 'debit', amount: 300.00 },    // 1200.00
                { type: 'credit', amount: 100.00 },   // 1300.00
                { type: 'debit', amount: 50.00 },     // 1250.00
                { type: 'credit', amount: 250.00 },   // 1500.00
                { type: 'debit', amount: 100.00 }     // 1400.00
            ];
            
            operations.forEach(op => {
                let balance = dataProgram.operation('READ');
                
                if (op.type === 'credit') {
                    balance += op.amount;
                    expectedBalance += op.amount;
                    dataProgram.operation('WRITE', balance);
                } else if (op.type === 'debit') {
                    if (balance >= op.amount) {
                        balance -= op.amount;
                        expectedBalance -= op.amount;
                        dataProgram.operation('WRITE', balance);
                    }
                }
                
                const currentBalance = dataProgram.operation('READ');
                expect(currentBalance).toBe(expectedBalance);
            });
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1400.00);
        });

        test('should handle operations with decimal amounts', () => {
            const operations = [
                { type: 'credit', amount: 123.45 },
                { type: 'debit', amount: 23.45 },
                { type: 'credit', amount: 50.50 },
                { type: 'debit', amount: 100.00 },
                { type: 'credit', amount: 49.50 }
            ];
            
            let expectedBalance = 1000.00;
            
            operations.forEach(op => {
                let balance = dataProgram.operation('READ');
                
                if (op.type === 'credit') {
                    balance += op.amount;
                    expectedBalance += op.amount;
                } else if (op.type === 'debit') {
                    if (balance >= op.amount) {
                        balance -= op.amount;
                        expectedBalance -= op.amount;
                    }
                }
                
                // Round to maintain currency precision
                balance = Math.round(balance * 100) / 100;
                expectedBalance = Math.round(expectedBalance * 100) / 100;
                
                dataProgram.operation('WRITE', balance);
            });
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBeCloseTo(1100.00, 2);
        });

        test('should handle mix of successful and failed transactions', () => {
            const operations = [
                { type: 'credit', amount: 500.00, shouldSucceed: true },
                { type: 'debit', amount: 2000.00, shouldSucceed: false }, // Insufficient funds
                { type: 'credit', amount: 300.00, shouldSucceed: true },
                { type: 'debit', amount: 1900.00, shouldSucceed: false }, // Insufficient funds
                { type: 'debit', amount: 800.00, shouldSucceed: true },
                { type: 'debit', amount: 1500.00, shouldSucceed: false }, // Insufficient funds
                { type: 'credit', amount: 500.00, shouldSucceed: true }
            ];
            
            let expectedBalance = 1000.00;
            
            operations.forEach(op => {
                let balance = dataProgram.operation('READ');
                let succeeded = false;
                
                if (op.type === 'credit') {
                    balance += op.amount;
                    expectedBalance += op.amount;
                    dataProgram.operation('WRITE', balance);
                    succeeded = true;
                } else if (op.type === 'debit') {
                    if (balance >= op.amount) {
                        balance -= op.amount;
                        expectedBalance -= op.amount;
                        dataProgram.operation('WRITE', balance);
                        succeeded = true;
                    }
                }
                
                expect(succeeded).toBe(op.shouldSucceed);
                
                const currentBalance = dataProgram.operation('READ');
                expect(currentBalance).toBe(expectedBalance);
            });
            
            // Final: 1000 + 500 + 300 - 800 + 500 = 1500
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1500.00);
        });
    });

    describe('Data Flow Integrity Across Operations', () => {
        test('should maintain correct data flow through read-modify-write cycles', () => {
            // Simulate complete data flow as per documentation
            const transactions = [
                { operation: 'VIEW', expectedBalance: 1000.00 },
                { operation: 'CREDIT', amount: 500.00, expectedBalance: 1500.00 },
                { operation: 'VIEW', expectedBalance: 1500.00 },
                { operation: 'DEBIT', amount: 300.00, expectedBalance: 1200.00 },
                { operation: 'VIEW', expectedBalance: 1200.00 },
                { operation: 'CREDIT', amount: 100.00, expectedBalance: 1300.00 },
                { operation: 'VIEW', expectedBalance: 1300.00 }
            ];
            
            transactions.forEach(transaction => {
                if (transaction.operation === 'VIEW') {
                    const balance = dataProgram.operation('READ');
                    expect(balance).toBe(transaction.expectedBalance);
                } else if (transaction.operation === 'CREDIT') {
                    let balance = dataProgram.operation('READ');
                    balance += transaction.amount;
                    dataProgram.operation('WRITE', balance);
                    expect(dataProgram.operation('READ')).toBe(transaction.expectedBalance);
                } else if (transaction.operation === 'DEBIT') {
                    let balance = dataProgram.operation('READ');
                    if (balance >= transaction.amount) {
                        balance -= transaction.amount;
                        dataProgram.operation('WRITE', balance);
                    }
                    expect(dataProgram.operation('READ')).toBe(transaction.expectedBalance);
                }
            });
        });
    });
});
