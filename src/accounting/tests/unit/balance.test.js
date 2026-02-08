/**
 * Balance and Data Integrity Tests
 * Test Cases: TC-VIEW-*, TC-DATA-*, TC-INIT-001
 */

const { DataProgram } = require('../../index');

describe('System Initialization Tests', () => {
    describe('TC-INIT-001: Verify initial balance is set correctly', () => {
        test('should start with initial balance of $1,000.00', () => {
            const dataProgram = new DataProgram();
            const balance = dataProgram.operation('READ');
            
            expect(balance).toBe(1000.00);
        });
    });
});

describe('View Balance Tests', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-VIEW-001: View initial balance', () => {
        test('should display initial balance of $1,000.00', () => {
            const balance = dataProgram.operation('READ');
            
            expect(balance).toBe(1000.00);
            expect(balance.toFixed(2)).toBe('1000.00');
        });
    });

    describe('TC-VIEW-002: View balance after credit operation', () => {
        test('should display updated balance after credit', () => {
            // Credit $500.00
            let balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            
            // View balance
            const newBalance = dataProgram.operation('READ');
            
            expect(newBalance).toBe(1500.00);
            expect(newBalance.toFixed(2)).toBe('1500.00');
        });
    });

    describe('TC-VIEW-003: View balance after debit operation', () => {
        test('should display updated balance after debit', () => {
            // Debit $300.00
            let balance = dataProgram.operation('READ');
            balance -= 300.00;
            dataProgram.operation('WRITE', balance);
            
            // View balance
            const newBalance = dataProgram.operation('READ');
            
            expect(newBalance).toBe(700.00);
            expect(newBalance.toFixed(2)).toBe('700.00');
        });
    });

    describe('TC-VIEW-004: View balance multiple times', () => {
        test('should return same balance on multiple reads without modifications', () => {
            const balance1 = dataProgram.operation('READ');
            const balance2 = dataProgram.operation('READ');
            const balance3 = dataProgram.operation('READ');
            const balance4 = dataProgram.operation('READ');
            
            expect(balance1).toBe(1000.00);
            expect(balance2).toBe(1000.00);
            expect(balance3).toBe(1000.00);
            expect(balance4).toBe(1000.00);
            expect(balance1).toBe(balance2);
            expect(balance2).toBe(balance3);
            expect(balance3).toBe(balance4);
        });
    });
});

describe('Data Integrity and Persistence Tests', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    describe('TC-DATA-001: Balance persists across operations within session', () => {
        test('should maintain balance across multiple operations', () => {
            // Credit $500.00
            let balance = dataProgram.operation('READ');
            balance += 500.00;
            dataProgram.operation('WRITE', balance);
            
            // Read again (simulating unrelated operation)
            const persistedBalance = dataProgram.operation('READ');
            
            expect(persistedBalance).toBe(1500.00);
        });
    });

    describe('TC-DATA-002: Verify read operation doesn\'t modify balance', () => {
        test('should not change balance after multiple read operations', () => {
            const initialBalance = dataProgram.operation('READ');
            
            // Read 10 times
            for (let i = 0; i < 10; i++) {
                dataProgram.operation('READ');
            }
            
            const finalBalance = dataProgram.operation('READ');
            
            expect(finalBalance).toBe(initialBalance);
            expect(finalBalance).toBe(1000.00);
        });
    });

    describe('TC-DATA-003: Balance format consistency', () => {
        test('should maintain consistent decimal format for various balances', () => {
            const testBalances = [1234.56, 0.00, 999999.99, 100.00, 50.50];
            
            testBalances.forEach(testBalance => {
                dataProgram.operation('WRITE', testBalance);
                const retrievedBalance = dataProgram.operation('READ');
                
                expect(retrievedBalance).toBe(testBalance);
                expect(retrievedBalance.toFixed(2)).toMatch(/^\d+\.\d{2}$/);
            });
        });
    });

    describe('TC-DATA-004: Verify no balance corruption', () => {
        test('should maintain mathematical accuracy across multiple operations', () => {
            let expectedBalance = 1000.00;
            
            // Perform 20 mixed operations
            const operations = [
                { type: 'add', amount: 100 },
                { type: 'subtract', amount: 50 },
                { type: 'add', amount: 200 },
                { type: 'subtract', amount: 75 },
                { type: 'add', amount: 300 },
                { type: 'subtract', amount: 125 },
                { type: 'add', amount: 400 },
                { type: 'subtract', amount: 150 },
                { type: 'add', amount: 500 },
                { type: 'subtract', amount: 200 },
                { type: 'add', amount: 100 },
                { type: 'subtract', amount: 50 },
                { type: 'add', amount: 250 },
                { type: 'subtract', amount: 175 },
                { type: 'add', amount: 300 },
                { type: 'subtract', amount: 100 },
                { type: 'add', amount: 150 },
                { type: 'subtract', amount: 75 },
                { type: 'add', amount: 200 },
                { type: 'subtract', amount: 100 }
            ];
            
            operations.forEach(op => {
                let balance = dataProgram.operation('READ');
                if (op.type === 'add') {
                    balance += op.amount;
                    expectedBalance += op.amount;
                } else {
                    balance -= op.amount;
                    expectedBalance -= op.amount;
                }
                dataProgram.operation('WRITE', balance);
            });
            
            const finalBalance = dataProgram.operation('READ');
            
            expect(finalBalance).toBe(expectedBalance);
            expect(finalBalance).toBe(2400.00); // Manually calculated expected result
        });
    });
});

describe('DataProgram READ/WRITE Operations', () => {
    let dataProgram;

    beforeEach(() => {
        dataProgram = new DataProgram();
    });

    test('READ operation should return current storage balance', () => {
        const balance = dataProgram.operation('READ');
        expect(balance).toBe(1000.00);
    });

    test('WRITE operation should update storage balance', () => {
        dataProgram.operation('WRITE', 1500.00);
        const balance = dataProgram.operation('READ');
        expect(balance).toBe(1500.00);
    });

    test('WRITE then READ should return written value', () => {
        const newBalance = 2500.75;
        dataProgram.operation('WRITE', newBalance);
        const retrievedBalance = dataProgram.operation('READ');
        expect(retrievedBalance).toBe(newBalance);
    });

    test('Multiple WRITEs should overwrite previous value', () => {
        dataProgram.operation('WRITE', 1500.00);
        dataProgram.operation('WRITE', 2000.00);
        dataProgram.operation('WRITE', 2500.00);
        
        const balance = dataProgram.operation('READ');
        expect(balance).toBe(2500.00);
    });
});
