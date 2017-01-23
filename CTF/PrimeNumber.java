import java.util.*;

public class PrimeNumber
{
public static void main(String args [])
{
Scanner sc = new Scanner(System.in);
int num1, num2;
//System.out.println("Please enter the first number");
num1 = 1;
//System.out.println("Please enter the Second number");
num2 = sc.nextInt();
//System.out.println("Prime number: ");
boolean found = false;
for (int i=(num2-1); i >= num1; i-- ){
	if (found == true) {
		break;
	}
	int j;

	for (j=2; j<i; j++){
		int n = i%j;
		if (n==0){
//System.out.print("not found " + i);
			found = false;
			break;
		}
	}
	if(i == j){
//		System.out.print(" found " + i);
		found = true;
		System.out.print(i);
	}
}
System.out.println();
}
}
