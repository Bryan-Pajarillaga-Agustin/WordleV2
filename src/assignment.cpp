// Labels

int legA = 2;
int legB = 3;
int legC = 4;
int legD = 5;
int legE = 6;
int legF = 7;
int legG = 8;
int legDP = 9;
int redLight = 10;
int blueLight = 11;
int greenLight = 12;
int button = 13;
int switchState = 0;

void setup(){
    pinMode(legA, OUTPUT);
    pinMode(legB, OUTPUT);
    pinMode(legC, OUTPUT);
    pinMode(legD, OUTPUT);
    pinMode(legE, OUTPUT);
    pinMode(legF, OUTPUT);
    pinMode(legG, OUTPUT);
    pinMode(legDP, OUTPUT);
    pinMode(redLight, OUTPUT);
    pinMode(blueLight, OUTPUT);
    pinMode(greenLight, OUTPUT);
    pinMode(button, INPUT);
}

int arrayNumberThree[6] = {
  legA, legB, legC, legD, legG, legDP
};
int arrayNumberTwo[6] = {
    legA, legB, legG, legD, legE, legDP
};
int arrayNumberOne[3] = {
    legB, legC, legDP
};
int arrayNumberZero[7] = {
    legA, legB, legC, legD, legE, legF, legDP
};

void loop(){
    switchState = digitalRead(button);
    if(switchState == HIGH){
        digitalWrite(legDP, LOW);

        analogWrite(redLight, 255);
        analogWrite(blueLight, 0);
        analogWrite(greenLight, 0);
        for(int i = 0; i < 6; i++){
            i < 5 ? digitalWrite(arrayNumberThree[i], LOW) : digitalWrite(arrayNumberThree[i], HIGH);
        }
        delay(1000);

        analogWrite(redLight, 0);
        analogWrite(blueLight, 255);
        analogWrite(greenLight, 0);
        for(int i = 0; i < 6; i++){
            i < 5 ? digitalWrite(arrayNumberTwo[i], LOW) : digitalWrite(arrayNumberTwo[i], HIGH);
        }
        delay(1000);


        analogWrite(redLight, 0);
        analogWrite(blueLight, 0);
        analogWrite(greenLight, 255);
        for(int i = 0; i < 3; i++){
            i < 2 ? digitalWrite(arrayNumberOne[i], LOW) : digitalWrite(arrayNumberOne[i], HIGH);
        }

        delay(1000);


        analogWrite(redLight, 255);
        analogWrite(blueLight, 0);
        analogWrite(greenLight, 255);
        for(int i = 0; i < 7; i++){
            i < 6 ? digitalWrite(arrayNumberZero[i], LOW) : digitalWrite(arrayNumberZero[i], HIGH);
        }

    }
}








// Labels

int legA = 2;
int legB = 3;
int legC = 4;
int legD = 5;
int legE = 6;
int legF = 7;
int legG = 8;
int legDP = 9;
int redLight = 10;
int blueLight = 11;
int greenLight = 12;
int button = 13;
int switchState = 0;

void setup(){
    pinMode(legA, OUTPUT);
    pinMode(legB, OUTPUT);
    pinMode(legC, OUTPUT);
    pinMode(legD, OUTPUT);
    pinMode(legE, OUTPUT);
    pinMode(legF, OUTPUT);
    pinMode(legG, OUTPUT);
    pinMode(legDP, OUTPUT);
    pinMode(redLight, OUTPUT);
    pinMode(blueLight, OUTPUT);
    pinMode(greenLight, OUTPUT);
    pinMode(button, INPUT);
}

int arrayNumberThree[6] = {
  legA, legB, legC, legD, legG, legDP
};
int arrayNumberTwo[6] = {
    legA, legB, legG, legD, legE, legDP
};
int arrayNumberOne[3] = {
    legB, legC, legDP
};
int arrayNumberZero[7] = {
    legA, legB, legC, legD, legE, legF, legDP
};

void loop(){
    switchState = digitalRead(button);
    if(switchState == HIGH){

        analogWrite(redLight, 255);
        analogWrite(blueLight, 0);
        analogWrite(greenLight, 0);
        digitalWrite(legA, LOW);
        digitalWrite(legB, LOW);
        digitalWrite(legC, LOW);
        digitalWrite(legD, LOW);
        digitalWrite(legE, HIGH);
        digitalWrite(legF, HIGH);
        digitalWrite(legG, LOW);
        digitalWrite(legDP, LOW);
        delay(1000);
    }
}