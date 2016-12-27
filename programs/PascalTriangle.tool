program Pascal {
	if(new PascalConstr().init(10)) {println("Ok");} else {println("Error");}
}

class PascalConstr {

	def init(n : Int) : Bool = {
	
		var triangle : String;
		var counterRow : Int;
		var counterCol : Int;
		var inter : Int;
		
		triangle = "";
		counterRow = 0;
		counterCol = 0;
		
		println("Pascal triangle with " + n + " lines.");
		
		while(counterRow < n) {
					
			while((counterCol-1) < counterRow) {
			
				inter = this.computePascal(counterRow, counterCol);
				triangle = triangle + inter + " ";
			
				counterCol = counterCol + 1;
			}
			
			println(triangle);
			triangle = "";
						
			counterRow = counterRow + 1;
			counterCol = 0;
		}
		
		return true;	
	}
	
	def computePascal(r : Int, c : Int) : Int = {
		
		var res : Int;
		
		if(c == 0 || c == r) {
			res = 1;
		} else {
			res = this.computePascal(r - 1, c - 1);
			res = res + this.computePascal(r - 1, c);
		}
		
		return res;
	}
}


