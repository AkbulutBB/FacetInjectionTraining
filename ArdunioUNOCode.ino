#include <Wire.h>
#include <LiquidCrystal_I2C.h>

// Define notes
#define NOTE_C4  262
#define NOTE_D4  294
#define NOTE_E4  330
#define NOTE_F4  349
#define NOTE_G4  392
#define NOTE_A4  440
#define NOTE_B4  494

// Initialize the I2C LCD (address 0x27, 16 columns, 2 rows)
LiquidCrystal_I2C lcd(0x27, 16, 2);

// Pin assignments
const int greenPinIn  = 2;    // Correct contact input on D2
const int redPinIn    = 3;    // Wrong contact input on D3
const int redPinOut   = 10;   // Red LED/relay
const int greenPinOut = 11;   // Green LED/relay

// Timing variables for serial output
unsigned long lastSerialUpdate = 0;
const unsigned long SERIAL_UPDATE_INTERVAL = 1000; // Print every 1 second

void setup() {
  Serial.begin(9600);
  Serial.println(F("System initialization started..."));

  // Initialize LCD
  lcd.init();
  lcd.backlight();
  
  // Configure pins - using INPUT mode since we have external pull-down resistors
  pinMode(greenPinIn, INPUT);
  pinMode(redPinIn, INPUT);
  pinMode(redPinOut, OUTPUT);
  pinMode(greenPinOut, OUTPUT);
  pinMode(9, OUTPUT); // Buzzer
  
  // Initial state
  digitalWrite(redPinOut, LOW);
  digitalWrite(greenPinOut, LOW);

  // Welcome message
  lcd.setCursor(0, 0);
  lcd.print("Sistem hazir");
  lcd.setCursor(0, 1);
  lcd.print("Girdi bekleniyor...");
  delay(2000);
  
  Serial.println(F("Setup completed"));
}

void loop() {
    static unsigned long lastChangeTime = 0;
    static bool lastRedState = false;
    static bool lastGreenState = false;
    
    // Read input states (HIGH when 5V present, LOW when not connected due to pull-down)
    bool greenState = digitalRead(greenPinIn);
    bool redState = digitalRead(redPinIn);
    
    // Only process state changes after debounce delay
    if (millis() - lastChangeTime < 50) {
        return;
    }
    
    // Print debug info on state change or interval
    if ((greenState != lastGreenState) || 
        (redState != lastRedState) || 
        (millis() - lastSerialUpdate >= SERIAL_UPDATE_INTERVAL)) {
            
        Serial.print(F("\nD2: "));
        Serial.print(greenState);
        Serial.print(F(" D3: "));
        Serial.println(redState);
        
        lastSerialUpdate = millis();
    }
    
    // State machine
    if (!redState && !greenState) {
        // Idle state - no contact
        digitalWrite(redPinOut, LOW);
        digitalWrite(greenPinOut, LOW);
        
        if (lastRedState || lastGreenState) {
            lcd.clear();
            lcd.setCursor(0, 0);
            lcd.print("Kontak saptanmadi.");
            lcd.setCursor(0, 1);
            lcd.print("Beklemede...");
        }
    }
    else if (redState) {
        // Wrong contact (D3 active)
        digitalWrite(redPinOut, HIGH);
        digitalWrite(greenPinOut, LOW);
        
        if (!lastRedState) {
            lcd.clear();
            lcd.setCursor(0, 0);
            lcd.print("Yanlis temas");
            errorSound();
        }
    }
    else if (greenState) {
        // Correct contact (D2 active)
        digitalWrite(redPinOut, LOW);
        digitalWrite(greenPinOut, HIGH);
        
        if (!lastGreenState) {
            lcd.clear();
            lcd.setCursor(0, 0);
            lcd.print("Dogru temas!");
            happyTune();
        }
    }
    
    // Update last states if changed
    if (greenState != lastGreenState || redState != lastRedState) {
        lastChangeTime = millis();
        lastGreenState = greenState;
        lastRedState = redState;
    }
    
    delay(10);
}

void happyTune() {
  tone(9, NOTE_C4, 300);
  delay(300);
  tone(9, NOTE_E4, 300);
  delay(300);
  tone(9, NOTE_G4, 300);
  delay(300);
  tone(9, NOTE_A4, 300);
  delay(300);
  tone(9, NOTE_B4, 400);
  delay(400);
  noTone(9);
}

void errorSound() {
  tone(9, NOTE_G4, 400);
  delay(400);
  tone(9, NOTE_E4, 600);
  delay(600);
  noTone(9);
}
