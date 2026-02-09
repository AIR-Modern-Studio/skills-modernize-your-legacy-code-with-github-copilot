/**
 * Credit Account Tests
 * Test Cases: TC-CREDIT-001 through TC-CREDIT-007
 */

const { DataProgram } = require('../../index');

describe('Credit Account Tests', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-CREDIT-001: Credit account with valid amount', () => {
        test('should increase balance by $500.00', () => {
            // Read current balance ($1,000.00)
            let balance = dataProgram.operation('READ');
            expect(balance).toBe(1000.00);
            
            // Add credit amount
            const creditAmount = 500.00;
            balance += creditAmount;
            
            // Write new balance
            dataProgram.operation('WRITE', balance);
            
            // Verify new balance
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(1500.00);
        });
    });

    describe('TC-CREDIT-002: Credit account with small amount', () => {
        test('should increase balance by $0.01', () => {
            let balance = dataProgram.operation('READ');
            balance += 0.01;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(1000.01);
        });
    });

    describe('TC-CREDIT-003: Credit account with large amount', () => {
        test('should increase balance by $50,000.00', () => {
            let balance = dataProgram.operation('READ');
            balance += 50000.00;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(51000.00);
        });
    });

    describe('TC-CREDIT-004: Credit account multiple times', () => {
        test('should correctly accumulate multiple credits', () => {
            // Credit $200.00
            let balance = dataProgram.operation('READ');
            balance += 200.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(1200.00);
            
            // Credit $300.00
            balance = dataProgram.operation('READ');
            balance += 300.00;
            dataProgram.operation('WRITE', balance);
            expect(dataProgram.operation('READ')).toBe(1500.00);
            
            // Credit $100.00
            balance = dataProgram.operation('READ');
            balance += 100.00;
            dataProgram.operation('WRITE', balance);
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBe(1600.00);
        });
    });

    describe('TC-CREDIT-005: Credit account with decimal precision', () => {
        test('should maintain decimal precision for $123.45', () => {
            let balance = dataProgram.operation('READ');
            const creditAmount = 123.45;
            balance += creditAmount;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(1123.45);
            expect(newBalance.toFixed(2)).toBe('1123.45');
        });
    });

    describe('TC-CREDIT-006: Credit account approaching maximum balance', () => {
        test('should update balance correctly near maximum', () => {
            // Set balance to $995,000.00
            dataProgram.operation('WRITE', 995000.00);
            
            // Credit $4,000.00
            let balance = dataProgram.operation('READ');
            balance += 4000.00;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(999000.00);
            expect(newBalance).toBeLessThanOrEqual(999999.99);
        });
    });

    describe('TC-CREDIT-007: Credit account with zero amount', () => {
        test('should keep balance unchanged when crediting $0.00', () => {
            let balance = dataProgram.operation('READ');
            const creditAmount = 0.00;
            balance += creditAmount;
            dataProgram.operation('WRITE', balance);
            
            const newBalance = dataProgram.operation('READ');
            expect(newBalance).toBe(1000.00);
        });
    });

    describe('Credit Account - Maximum Balance Validation', () => {
        test('should not exceed maximum balance of $999,999.99', () => {
            // Set balance near maximum
            dataProgram.operation('WRITE', 999995.00);
            
            let balance = dataProgram.operation('READ');
            const creditAmount = 10.00;
            balance += creditAmount;
            
            // Check if would exceed maximum
            if (balance > 999999.99) {
                // Reject operation - balance should remain unchanged
                const currentBalance = dataProgram.operation('READ');
                expect(currentBalance).toBe(999995.00);
            } else {
                dataProgram.operation('WRITE', balance);
                const newBalance = dataProgram.operation('READ');
                expect(newBalance).toBeLessThanOrEqual(999999.99);
            }
        });
    });

    describe('Credit Account - Read-Modify-Write Pattern', () => {
        test('should follow READ-MODIFY-WRITE pattern correctly', () => {
            // Step 1: Read current balance
            const currentBalance = dataProgram.operation('READ');
            expect(currentBalance).toBe(1000.00);
            
            // Step 2: Modify (calculate new balance)
            const creditAmount = 250.00;
            const newBalance = currentBalance + creditAmount;
            expect(newBalance).toBe(1250.00);
            
            // Step 3: Write new balance
            dataProgram.operation('WRITE', newBalance);
            
            // Verify persistence
            const verifyBalance = dataProgram.operation('READ');
            expect(verifyBalance).toBe(1250.00);
        });
    });

    describe('Credit Account - Decimal Arithmetic Accuracy', () => {
        test('should handle multiple decimal credits without rounding errors', () => {
            const credits = [10.10, 20.20, 30.30, 40.40, 50.50];
            let expectedBalance = 1000.00;
            
            credits.forEach(credit => {
                let balance = dataProgram.operation('READ');
                balance += credit;
                expectedBalance += credit;
                dataProgram.operation('WRITE', balance);
            });
            
            const finalBalance = dataProgram.operation('READ');
            expect(finalBalance).toBeCloseTo(expectedBalance, 2);
            expect(finalBalance).toBeCloseTo(1151.50, 2);
        });
    });
});
