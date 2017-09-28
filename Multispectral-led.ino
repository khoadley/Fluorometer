int REP = 5;
int del = 150;
int btwnflash = 5000;
int LED447nm = 12;
int LED505nm = 11;
int LED530nm = 10;
int LED597nm = 9;
int LED615nm = 8;
int LED625nm = 7;
int TRIG = 6;

   
void setup() 
{
  //analogReference(EXTERNAL); //
  Serial.begin(9600); //Serial.println("Adafruit Analog Light Sensor Test");
  pinMode(LED447nm, OUTPUT);
  pinMode(LED505nm, OUTPUT);
  pinMode(LED530nm, OUTPUT);
  pinMode(LED597nm, OUTPUT);
  pinMode(LED615nm, OUTPUT);
  pinMode(LED625nm, OUTPUT);
  pinMode(TRIG, OUTPUT);
  
}
   
void loop() 
{
    if (Serial.available())
    {
         char inputValue = Serial.read();
         if(inputValue =='a') 
         {  
            LED447();
         }
         if(inputValue =='s') 
         {  
            LED505();
         }
         if(inputValue =='d') 
         {  
            LED530();
         }
         if(inputValue =='f') 
         {  
            LED597();
         }
         if(inputValue =='g') 
         {  
            LED615();
         }
         if(inputValue =='h') 
         {  
            LED625();
         }
         else
         {
         }
     }
}


void LED447()
{
        for (int i=0;i<REP;i++)
        { 
          digitalWrite(TRIG, HIGH);
          delayMicroseconds(10);
          digitalWrite(TRIG, LOW);
          digitalWrite(LED447nm, HIGH);
          delayMicroseconds(del);
          digitalWrite(LED447nm, LOW);
          delay(btwnflash);
        }
}

void LED505()
{
        for (int i=0;i<REP;i++)
        { 
          digitalWrite(TRIG, HIGH);
          delayMicroseconds(10);
          digitalWrite(TRIG, LOW);
          digitalWrite(LED505nm, HIGH);
          delayMicroseconds(del);
          digitalWrite(LED505nm, LOW);
          delay(btwnflash);
        }
}

void LED530()
{
        for (int i=0;i<REP;i++)
        { 
          digitalWrite(TRIG, HIGH);
          delayMicroseconds(10);
          digitalWrite(TRIG, LOW);
          digitalWrite(LED530nm, HIGH);
          delayMicroseconds(del);
          digitalWrite(LED530nm, LOW);
          delay(btwnflash);
        }
}

void LED597()
{
        for (int i=0;i<REP;i++)
        { 
          digitalWrite(TRIG, HIGH);
          delayMicroseconds(10);
          digitalWrite(TRIG, LOW);
          digitalWrite(LED597nm, HIGH);
          delayMicroseconds(del);
          digitalWrite(LED597nm, LOW);
          delay(btwnflash);
        }
}

void LED615()
{
        for (int i=0;i<REP;i++)
        { 
          digitalWrite(TRIG, HIGH);
          delayMicroseconds(10);
          digitalWrite(TRIG, LOW);
          digitalWrite(LED615nm, HIGH);
          delayMicroseconds(del);
          digitalWrite(LED615nm, LOW);
          delay(btwnflash);
        }
}

void LED625()
{
        for (int i=0;i<REP;i++)
        { 
          digitalWrite(TRIG, HIGH);
          delayMicroseconds(10);
          digitalWrite(TRIG, LOW);
          digitalWrite(LED625nm, HIGH);
          delayMicroseconds(del);
          digitalWrite(LED625nm, LOW);
          delay(btwnflash);
        }
}


