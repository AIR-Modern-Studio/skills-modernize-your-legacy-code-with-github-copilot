#!/usr/bin/env node

/**
 * Account Management System - Modernized from COBOL Legacy Code
 * 
 * This Node.js application is a direct port of the three-tier COBOL system:
 * - MainProgram (main.cob): User interface and menu control
 * - Operations (operations.cob): Business logic and validation
 * - DataProgram (data.cob): Data storage and retrieval
 * 
 * Business Rules Preserved:
 * - Initial balance: $1,000.00
 * - Maximum balance/transaction: $999,999.99
 * - Insufficient funds protection for debits
 * - Read-Modify-Write pattern for data integrity
 */

const readline = require('readline');

// ============================================================================
// DATA LAYER (DataProgram equivalent)
// ============================================================================

/**
 * DataProgram - Manages data persistence for account balance
 * Equivalent to data.cob
 */
class DataProgram {
    constructor() {
        // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
        this.storageBalance = 1000.00;
    }

    /**
     * Performs READ or WRITE operations on the balance
     * @param {string} operationType - 'READ' or 'WRITE'
     * @param {number|null} balance - Balance value for WRITE operations
     * @returns {number|null} Balance for READ operations, null for WRITE
     */
    operation(operationType, balance = null) {
        if (operationType === 'READ') {
            // MOVE STORAGE-BALANCE TO BALANCE
            return this.storageBalance;
        } else if (operationType === 'WRITE' && balance !== null) {
            // MOVE BALANCE TO STORAGE-BALANCE
            this.storageBalance = balance;
            return null;
        }
        return null;
    }
}

// ============================================================================
// BUSINESS LOGIC LAYER (Operations equivalent)
// ============================================================================

/**
 * Operations - Handles all business logic and account transactions
 * Equivalent to operations.cob
 */
class Operations {
    constructor(dataProgram, rl) {
        this.dataProgram = dataProgram;
        this.rl = rl;
    }

    /**
     * View current balance (TOTAL operation)
     * Equivalent to TOTAL operation in operations.cob
     */
    async viewBalance() {
        // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        const finalBalance = this.dataProgram.operation('READ');
        
        // DISPLAY "Current balance: " FINAL-BALANCE
        console.log(`Current balance: $${finalBalance.toFixed(2)}`);
    }

    /**
     * Credit account (add funds)
     * Equivalent to CREDIT operation in operations.cob
     */
    async creditAccount() {
        // DISPLAY "Enter credit amount: "
        // ACCEPT AMOUNT
        const amount = await this.promptForAmount('Enter credit amount: $');
        
        if (amount === null) return;

        // Step 1: Read current balance
        // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        let finalBalance = this.dataProgram.operation('READ');
        
        // Step 2: Calculate new balance
        // ADD AMOUNT TO FINAL-BALANCE
        finalBalance += amount;
        
        // Enforce maximum balance constraint (PIC 9(6)V99 = max 999,999.99)
        if (finalBalance > 999999.99) {
            console.log('Error: Balance would exceed maximum allowed ($999,999.99)');
            return;
        }
        
        // Step 3: Write new balance
        // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        this.dataProgram.operation('WRITE', finalBalance);
        
        // DISPLAY "Amount credited. New balance: " FINAL-BALANCE
        console.log(`Amount credited. New balance: $${finalBalance.toFixed(2)}`);
    }

    /**
     * Debit account (withdraw funds)
     * Equivalent to DEBIT operation in operations.cob
     */
    async debitAccount() {
        // DISPLAY "Enter debit amount: "
        // ACCEPT AMOUNT
        const amount = await this.promptForAmount('Enter debit amount: $');
        
        if (amount === null) return;

        // Step 1: Read current balance
        // CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        let finalBalance = this.dataProgram.operation('READ');
        
        // Step 2: Validate sufficient funds
        // IF FINAL-BALANCE >= AMOUNT
        if (finalBalance >= amount) {
            // Step 3: Calculate new balance
            // SUBTRACT AMOUNT FROM FINAL-BALANCE
            finalBalance -= amount;
            
            // Step 4: Write new balance
            // CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
            this.dataProgram.operation('WRITE', finalBalance);
            
            // DISPLAY "Amount debited. New balance: " FINAL-BALANCE
            console.log(`Amount debited. New balance: $${finalBalance.toFixed(2)}`);
        } else {
            // DISPLAY "Insufficient funds for this debit."
            console.log('Insufficient funds for this debit.');
        }
    }

    /**
     * Helper method to prompt user for amount with validation
     * @param {string} prompt - The prompt message
     * @returns {Promise<number|null>} The amount or null if invalid
     */
    async promptForAmount(prompt) {
        const input = await this.question(prompt);
        const amount = parseFloat(input);
        
        // Validate input
        if (isNaN(amount) || amount <= 0) {
            console.log('Invalid amount. Please enter a positive number.');
            return null;
        }
        
        // Enforce maximum transaction constraint (PIC 9(6)V99)
        if (amount > 999999.99) {
            console.log('Amount exceeds maximum allowed ($999,999.99)');
            return null;
        }
        
        // Round to 2 decimal places for currency precision
        return Math.round(amount * 100) / 100;
    }

    /**
     * Wrapper for readline question to return a promise
     * @param {string} query - The question to ask
     * @returns {Promise<string>} The user's answer
     */
    question(query) {
        return new Promise(resolve => {
            this.rl.question(query, resolve);
        });
    }
}

// ============================================================================
// PRESENTATION LAYER (MainProgram equivalent)
// ============================================================================

/**
 * MainProgram - Main entry point and user interface
 * Equivalent to main.cob
 */
class MainProgram {
    constructor() {
        // Create readline interface for user input
        this.rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout
        });

        // Initialize data and operations layers
        this.dataProgram = new DataProgram();
        this.operations = new Operations(this.dataProgram, this.rl);
        
        // CONTINUE-FLAG PIC X(3) VALUE 'YES'
        this.continueFlag = true;
    }

    /**
     * Display the main menu
     * Equivalent to menu display in main.cob
     */
    displayMenu() {
        console.log('--------------------------------');
        console.log('Account Management System');
        console.log('1. View Balance');
        console.log('2. Credit Account');
        console.log('3. Debit Account');
        console.log('4. Exit');
        console.log('--------------------------------');
    }

    /**
     * Main program logic loop
     * Equivalent to MAIN-LOGIC paragraph in main.cob
     */
    async run() {
        // PERFORM UNTIL CONTINUE-FLAG = 'NO'
        while (this.continueFlag) {
            this.displayMenu();
            
            // DISPLAY "Enter your choice (1-4): "
            // ACCEPT USER-CHOICE
            const choice = await this.getUserChoice();
            
            // EVALUATE USER-CHOICE
            await this.processChoice(choice);
        }
        
        // DISPLAY "Exiting the program. Goodbye!"
        console.log('Exiting the program. Goodbye!');
        this.rl.close();
        
        // STOP RUN
        process.exit(0);
    }

    /**
     * Get user's menu choice
     * @returns {Promise<string>} The user's choice
     */
    async getUserChoice() {
        return new Promise(resolve => {
            this.rl.question('Enter your choice (1-4): ', resolve);
        });
    }

    /**
     * Process the user's menu choice
     * Equivalent to EVALUATE block in main.cob
     * @param {string} choice - The user's menu selection
     */
    async processChoice(choice) {
        const userChoice = parseInt(choice, 10);
        
        switch (userChoice) {
            case 1:
                // WHEN 1
                // CALL 'Operations' USING 'TOTAL '
                await this.operations.viewBalance();
                break;
                
            case 2:
                // WHEN 2
                // CALL 'Operations' USING 'CREDIT'
                await this.operations.creditAccount();
                break;
                
            case 3:
                // WHEN 3
                // CALL 'Operations' USING 'DEBIT '
                await this.operations.debitAccount();
                break;
                
            case 4:
                // WHEN 4
                // MOVE 'NO' TO CONTINUE-FLAG
                this.continueFlag = false;
                break;
                
            default:
                // WHEN OTHER
                // DISPLAY "Invalid choice, please select 1-4."
                console.log('Invalid choice, please select 1-4.');
                break;
        }
        
        // Add blank line for readability between operations
        if (this.continueFlag) {
            console.log();
        }
    }
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

// Only run the application if this file is executed directly (not imported)
if (require.main === module) {
    const app = new MainProgram();
    app.run().catch(error => {
        console.error('Application error:', error);
        process.exit(1);
    });
}

// Export classes for testing
module.exports = {
    DataProgram,
    Operations,
    MainProgram
};
