/**
 * Edge Cases and Boundary Tests
 * Test Cases: TC-EDGE-001 through TC-EDGE-005
 */

const { DataProgram } = require('../../index');

describe('Edge Cases and Boundary Tests', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-EDGE-001: Test with maximum valid balance', () => {
        test('should handle maximum balance of $999,999.99', () => {
            const maxBalance = 999999.99;
            dataProgram.operation('WRITE', maxBalance);
            
            // View balance
            const balance = dataProgram.operation('READ');
            expect(balance).toBe(maxBalance);
            expect(balance.toFixed(2)).toBe('999999.99');
            
            // Attempt to debit small amount (should succeed)
            const debitAmount = 100.00;
            if (balance >= debitAmount) {
                const newBalance = balance - debitAmount;
                dataProgram.operation('WRITE', newBalance);
            }
            
            const afterDebit = dataProgram.operation('READ');
            expect(afterDebit).toBe(999899.99);
        });

        test('should display maximum balance correctly', () => {
            dataProgram.operation('WRITE', 999999.99);
            const balance = dataProgram.operation('READ');
            
            expect(balance).toBe(999999.99);
            expect(balance).toBeLessThanOrEqual(999999.99);
        });
    });

    describe('TC-EDGE-002: Test with minimum valid balance', () => {
        test('should handle minimum balance of $0.00', () => {
            dataProgram.operation('WRITE', 0.00);
            
            // View balance
            const balance = dataProgram.operation('READ');
            expect(balance).toBe(0.00);
            expect(balance.toFixed(2)).toBe('0.00');
            
            // Credit operations should work
            let newBalance = balance + 100.00;
            dataProgram.operation('WRITE', newBalance);
            expect(dataProgram.operation('READ')).toBe(100.00);
            
            // Debit operations should be rejected
            const currentBalance = dataProgram.operation('READ');
            const debitAmount = 200.00;
            
            if (currentBalance >= debitAmount) {
                dataProgram.operation('WRITE', currentBalance - debitAmount);
            }
            // Rejected due to insufficient funds
            
            expect(dataProgram.operation('READ')).toBe(100.00);
        });

        test('should handle zero balance operations', () => {
            dataProgram.operation('WRITE', 0.00);
            
            const balance = dataProgram.operation('READ');
            expect(balance).toBe(0.00);
            
            // Can credit from zero
            dataProgram.operation('WRITE', balance + 500.00);
            expect(dataProgram.operation('READ')).toBe(500.00);
            
            // Cannot debit from zero
            dataProgram.operation('WRITE', 0.00);
            const zeroBalance = dataProgram.operation('READ');
            
            if (zeroBalance >= 0.01) {
                dataProgram.operation('WRITE', zeroBalance - 0.01);
            }
            
            expect(dataProgram.operation('READ')).toBe(0.00);
        });
    });

    describe('TC-EDGE-003: Test with precisely calculated balances', () => {
        test('should not introduce floating-point errors', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Credit $333.33
            let balance = startingBalance + 333.33;
            balance = Math.round(balance * 100) / 100; // Round to 2 decimals
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBeCloseTo(1333.33, 2);
            
            // Debit $333.33
            balance = dataProgram.operation('READ');
            balance -= 333.33;
            balance = Math.round(balance * 100) / 100;
            dataProgram.operation('WRITE', balance);
            
            // Balance should return to $1,000.00
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBeCloseTo(1000.00, 2);
            expect(Math.abs(finalBalance - 1000.00)).toBeLessThan(0.01); // Within 1 cent
        });

        test('should handle repeating decimals correctly', () => {
            dataProgram.operation('WRITE', 100.00);
            
            // Add 1/3 three times (0.33 + 0.33 + 0.34 = 1.00)
            let balance = dataProgram.operation('READ');
            balance += 0.33;
            balance = Math.round(balance * 100) / 100;
            dataProgram.operation('WRITE', balance);
            
            balance = dataProgram.operation('READ');
            balance += 0.33;
            balance = Math.round(balance * 100) / 100;
            dataProgram.operation('WRITE', balance);
            
            balance = dataProgram.operation('READ');
            balance += 0.34; // Compensate for rounding
            balance = Math.round(balance * 100) / 100;
            dataProgram.operation('WRITE', balance);
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBeCloseTo(101.00, 2);
        });
    });

    describe('TC-EDGE-004: Rapid sequential operations', () => {
        test('should handle 50 rapid operations correctly', () => {
            let expectedBalance = 1000.00;
            
            // Perform 50 operations (25 credits, 25 debits)
            for (let i = 0; i < 25; i++) {
                // Credit
                let balance = dataProgram.operation('READ');
                balance += 10.00;
                expectedBalance += 10.00;
                dataProgram.operation('WRITE', balance);
                
                // Debit
                balance = dataProgram.operation('READ');
                if (balance >= 5.00) {
                    balance -= 5.00;
                    expectedBalance -= 5.00;
                    dataProgram.operation('WRITE', balance);
                }
            }
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(expectedBalance);
            expect(finalBalance).toBe(1125.00); // 1000 + (25 * 10) - (25 * 5)
        });

        test('should maintain consistency under rapid operation load', () => {
            const operations = 100;
            let operationCount = 0;
            
            for (let i = 0; i < operations; i++) {
                let balance = dataProgram.operation('READ');
                
                if (i % 2 === 0) {
                    // Even: credit $50
                    balance += 50.00;
                    dataProgram.operation('WRITE', balance);
                    operationCount++;
                } else {
                    // Odd: debit $25
                    if (balance >= 25.00) {
                        balance -= 25.00;
                        dataProgram.operation('WRITE', balance);
                        operationCount++;
                    }
                }
            }
            
            expect(operationCount).toBe(operations);
            
            // 100 operations: 50 credits of $50, 50 debits of $25
            // Net: (50 * 50) - (50 * 25) = 2500 - 1250 = +1250
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(2250.00); // 1000 + 1250
        });
    });

    describe('TC-EDGE-005: Test balance after reducing to zero then crediting', () => {
        test('should allow operations after balance reaches zero', () => {
            const startingBalance = dataProgram.operation('READ');
            expect(startingBalance).toBe(1000.00);
            
            // Step 1: Debit $1,000.00 (reduce to zero)
            let balance = dataProgram.operation('READ');
            if (balance >= 1000.00) {
                balance -= 1000.00;
                dataProgram.operation('WRITE', balance);
            }
            
            const zeroBalance = dataProgram.operation('READ');
            expect(zeroBalance).toBe(0.00);
            
            // Step 2: Credit $500.00
            balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            
            // Step 3: View balance
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(500.00);
            
            // Verify operations work correctly from zero
            expect(finalBalance).toBeGreaterThan(0);
        });

        test('should handle multiple zero-balance recoveries', () => {
            // Scenario: Balance goes to zero multiple times
            
            // Round 1: Start at $1000, go to $0, then $300
            let balance = dataProgram.operation('READ');
            balance -= 1000.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(0.00);
            
            balance = dataProgram.operation('READ');
            balance += 300.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(300.00);
            
            // Round 2: Go to $0 again, then $150
            balance = dataProgram.operation('READ');
            balance -= 300.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(0.00);
            
            balance = dataProgram.operation('READ');
            balance += 150.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(150.00);
        });
    });

    describe('Boundary Tests - Decimal Precision Limits', () => {
        test('should handle smallest currency unit ($0.01)', () => {
            dataProgram.operation('WRITE', 0.01);
            
            const balance = dataProgram.operation('READ');
            expect(balance).toBe(0.01);
            expect(balance.toFixed(2)).toBe('0.01');
            
            // Can debit exact amount
            if (balance >= 0.01) {
                dataProgram.operation('WRITE', balance - 0.01);
            }
            
            expect(dataProgram.operation('READ')).toBe(0.00);
        });

        test('should handle large decimal operations', () => {
            const largeAmount = 500000.50;
            dataProgram.operation('WRITE', largeAmount);
            
            let balance = dataProgram.operation('READ');
            expect(balance).toBe(largeAmount);
            
            // Large credit
            balance += 400000.49;
            dataProgram.operation('WRITE', balance);
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBeCloseTo(900000.99, 2);
            expect(finalBalance).toBeLessThanOrEqual(999999.99);
        });
    });

    describe('Boundary Tests - Maximum Transaction Amounts', () => {
        test('should handle maximum transaction amount', () => {
            dataProgram.operation('WRITE', 999999.99);
            
            const balance = dataProgram.operation('READ');
            expect(balance).toBe(999999.99);
            
            // Maximum possible debit
            if (balance >= 999999.99) {
                dataProgram.operation('WRITE', balance - 999999.99);
            }
            
            expect(dataProgram.operation('READ')).toBe(0.00);
        });

        test('should handle maximum credit from zero', () => {
            dataProgram.operation('WRITE', 0.00);
            
            let balance = dataProgram.operation('READ');
            balance += 999999.99;
            dataProgram.operation('WRITE', balance);
            
            const maxBalance = dataProgram.operation('READ');
            expect(maxBalance).toBe(999999.99);
        });
    });

    describe('Boundary Tests - Near-Boundary Operations', () => {
        test('should handle operations near zero boundary', () => {
            const nearZeroBalances = [0.01, 0.05, 0.10, 0.50, 0.99];
            
            nearZeroBalances.forEach(testBalance => {
                dataProgram.operation('WRITE', testBalance);
                const balance = dataProgram.operation('READ');
                
                expect(balance).toBeCloseTo(testBalance, 2);
                expect(balance).toBeGreaterThan(0);
                expect(balance).toBeLessThan(1);
            });
        });

        test('should handle operations near maximum boundary', () => {
            const nearMaxBalances = [999999.99, 999999.50, 999999.00, 999990.00, 999900.00];
            
            nearMaxBalances.forEach(testBalance => {
                dataProgram.operation('WRITE', testBalance);
                const balance = dataProgram.operation('READ');
                
                expect(balance).toBeCloseTo(testBalance, 2);
                expect(balance).toBeLessThanOrEqual(999999.99);
            });
        });
    });

    describe('Stress Test - Data Consistency', () => {
        test('should maintain consistency across 100 random operations', () => {
            let expectedBalance = 1000.00;
            const operations = [];
            
            // Generate 100 random but balanced operations
            for (let i = 0; i < 50; i++) {
                const creditAmount = Math.floor(Math.random() * 100) + 1; // 1-100
                const debitAmount = Math.floor(Math.random() * 50) + 1;   // 1-50
                
                operations.push({ type: 'credit', amount: creditAmount });
                operations.push({ type: 'debit', amount: debitAmount });
            }
            
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
            });
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(expectedBalance);
            expect(finalBalance).toBeGreaterThanOrEqual(0);
        });
    });
});
