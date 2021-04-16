#include <Time.h>  
#include <Wire.h>  
#include <DS1307RTC.h>  // a basic DS1307 library that returns time as a time_t
#include <TimeAlarms.h>

int ledAnalogOne[] = {3, 5, 6}; //the three pins of the first analog LED 3 = redPin, 5 = greenPin, 6 = bluePin
int ledAnalogTwo[] = {9, 10, 11};                                //These pins must be PWM
int motorPin1 = 4;
int motorPin2 = 8;
int feeder;

int lastFour[] = {0, 1, 0, 1};
boolean same;

const byte RED[] = {255, 0, 0}; 
const byte ORANGE[] = {83, 4, 0}; 
const byte YELLOW[] = {255, 255, 0}; 
const byte GREEN[] = {0, 255, 0}; 
const byte BLUE[] = {0, 0, 255}; 
const byte INDIGO[] = {4, 0, 19}; 
const byte VIOLET[] = {23, 0, 22}; 
const byte CYAN[] = {0, 255, 255}; 
const byte MAGENTA[] = {255, 0, 255}; 
const byte WHITE[] = {255, 255, 255}; 
const byte BLACK[] = {0, 0, 0}; 
const byte TEAL[] = {0, 128, 128};
const byte CRIMSON[] = {200, 20, 60};
const byte PURPLE[] = {128, 0, 128};
const byte ORANGEWEB[] = {255, 165, 0};
const byte GOLD[] = {255, 215, 0};
const byte LINDARANGE[] = {255, 60, 0};

void setup()  {
randomSeed(analogRead(0));
pinMode(motorPin1, OUTPUT);             //set the motor pin as output
pinMode(motorPin2, OUTPUT);             //set the motor pin as output
for(int i = 0; i < 3; i++){
   pinMode(ledAnalogOne[i], OUTPUT);   //Set the three LED pins as outputs
  }
  setColor(ledAnalogOne, BLACK);  //Turn off led 1
   setColor(ledAnalogTwo, BLACK);  //Turn off led 2
  Serial.begin(9600);
  while (!Serial) ; // wait until Arduino Serial Monitor opens
  setSyncProvider(RTC.get);   // the function to get the time from the RTC
  if(timeStatus()!= timeSet) 
     Serial.println("Unable to sync with the RTC");
  else
     Serial.println("RTC has set the system time");
  Alarm.alarmRepeat(8,27,0, Alarm1); //B4
  Alarm.alarmRepeat(11,27,0, Alarm2); //B4
  Alarm.alarmRepeat(14,27,00, Alarm3); //B4
  Alarm.alarmRepeat(17,27,0, Alarm4); //B4

}

void loop()
{
  if (timeStatus() == timeSet) {
    digitalClockDisplay();
    
    
  } else {
    Serial.println("The time has not been set.  Please run the Time");
    Serial.println("TimeRTCSet example, or DS1307RTC SetTime example.");
    Serial.println();
    delay(4000);
  }
   Alarm.delay(1000);
}

void digitalClockDisplay(){
  // digital clock display of the time
  Serial.print(hour());
  printDigits(minute());
  printDigits(second());
  Serial.print(" ");
  Serial.print(day());
  Serial.print(" ");
  Serial.print(month());
  Serial.print(" ");
  Serial.print(year()); 
  Serial.println(); 
}

void printDigits(int digits){
  // utility function for digital clock display: prints preceding colon and leading 0
  Serial.print(":");
  if(digits < 10)
    Serial.print('0');
  Serial.print(digits);
}

void Alarm1(){
 runAll();
}
void Alarm2(){
 runAll();
}
void Alarm3(){
 runAll();
}
void Alarm4(){
 runAll();
}

void setColor(int* led, byte* color){
 for(int i = 0; i < 3; i++){             //iterate through each of the three pins (red green blue)
   analogWrite(led[i], 255 - color[i]);  //set the analog output value of each pin to the input value (ie led[0] (red pin) to 255- color[0] (red input color)
                                         //we use 255 - the value because our RGB LED is common anode, this means a color is full on when we output analogWrite(pin, 0)
                                         //and off when we output analogWrite(pin, 255). 
 }
}

/* A version of setColor that takes a predefined color (neccesary to allow const int pre-defined colors */
void setColor(int* led, const byte* color){
 byte tempByte[] = {color[0], color[1], color[2]};
 setColor(led, tempByte);
}

void runAll(){
  same = 1;
  while (same == 1) { // we go through the while loop at least once to ensure that at least one value in lastFour[] array != newValue,
                      // which is randomly generated within the while loop
                      // we keep looping until 'newValue' is not the same as the values in the lastFour[] array
   feeder = random(2); // initialize 'newValue' with a random value of 0,1
   for (int index = 0; index < 4; index++) { // step through lastFour[] array and compare each value with 'newValue'
     // if a value in array isn't equal to newValue, set 'same' to False
     if (feeder != lastFour[index]) {same = 0;}
   } // end for (int index = 0; index < 4; index++)
  } // end while (same == 1)
  
  // we have ensured that at least one value in lastFour[] array doesn't equal 'newValue'
  // now update the lastFour[] array and use 'newValue'
  lastFour[0] = lastFour[1];
  lastFour[1] = lastFour[2];
  lastFour[2] = lastFour[3];
  lastFour[3] = feeder;
  for (int i = 0; i < 4; i++) {
    Serial.print(lastFour[i]);
    Serial.print(", ");
  }
  Serial.println("");
if (feeder==0)
{
setColor(ledAnalogOne, LINDARANGE); //This one is the color that will feed
setColor(ledAnalogTwo, CYAN);
delay(3000);
setColor(ledAnalogOne, BLACK);
setColor(ledAnalogTwo, BLACK);
delay(1000);
digitalWrite(motorPin1, HIGH);
delay(100);
digitalWrite(motorPin1, LOW);
delay(100);
digitalWrite(motorPin1, HIGH);
delay(100);
digitalWrite(motorPin1, LOW);
}
else if (feeder==1)
{
setColor(ledAnalogOne, CYAN);
setColor(ledAnalogTwo, LINDARANGE); // This one is the color that will feed
delay(3000);
setColor(ledAnalogOne, BLACK);
setColor(ledAnalogTwo, BLACK);
delay(1000);
digitalWrite(motorPin2, HIGH);
delay(100);
digitalWrite(motorPin2, LOW);
delay(100);
digitalWrite(motorPin2, HIGH);
delay(100);
digitalWrite(motorPin2, LOW);
}
}
