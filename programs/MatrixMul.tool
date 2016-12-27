program MatrixMul {
	if(new MatrixM().start()) {println("Ok");} else {println("Error");}
}

class MatrixM {

	var m1 : Int[];
	var row1 : Int;
	var col1 : Int;
	
	var m2 : Int[];
	var row2 : Int;
	var col2 : Int;
	
	var m3 : Int[];
	var row3 : Int;
	var col3 : Int;
	
	def start() : Bool = {
	
		do(this.init2());
		//do(this.init1(2, 2, 2, 2));
		do(this.mulMatrix());
		do(this.printAll());
	
		return true;
	}
	
	
	def init1(r1: Int, c1: Int, r2: Int, c2: Int) : Int = {
	
		var r : Int;
		var c : Int;
		var counter : Int;
		var index : Int;
	
		m1 = new Int[r1 * c1];
		m2 = new Int[r2 * c2];
		
		row1 = r1;
		col1 = c1;
		row2 = r2;
		col2 = c2;
		
		row3 = row1;
		col3 = col2;
		m3 = new Int[row3 * col3];
		
		counter = 0;
		r = 0;
		c = 0;
		
		while(r < r1) {
		
			while(c < c1) {
			
				index = r * c1 + c;
				
				m1[index] = counter;
				counter = counter + 1;
				
				c = c + 1;
			}
			
			c = 0;
			r = r + 1;
		}
		
		r = 0;
		c = 0;
		
		while(r < r2) {
		
			while(c < c2) {
			
				index = r * c2 + c;
									
				m2[index] = counter;
				counter = counter + 1;
				
				c = c + 1;
			}	
			
			c = 0;
			r = r + 1;
		}
		
		
		return 0;
	}
	
	def init2() : Int = {
	
		row1 = 2;
		col1 = 2;
		row2 = 2;
		col2 = 2;
		row3 = 2;
		col3 = 2;
		
		m1 = new Int[4];
		m2 = new Int[4];
		m3 = new Int[4];
		
		m1[0] = 2; m1[1] = 1; m1[2] = 3; m1[3] = 4;
		m2[0] = 4; m2[1] = 1; m2[2] = 5; m2[3] = 3;
		
		return 0;
	}

	def mulMatrix() : Int = {
	
		var r : Int;
		var cFirstMatr : Int;
		var cSecondMatr : Int;
		var indexM1 : Int;
		var indexM2 : Int;
		var indexM3 : Int;
		
		r = 0;
		cFirstMatr = 0;
		cSecondMatr = 0;
		
		while(r < row1) {
			
			while(cSecondMatr < col2) {
			
				indexM3 = r * col3 + cSecondMatr;			
			
				while(cFirstMatr < col1) {
				
					indexM1 = r * col1 + cFirstMatr;
					indexM2 = cSecondMatr + cFirstMatr * col2;
					
					m3[indexM3] = m3[indexM3] + m1[indexM1] * m2[indexM2];
				
					cFirstMatr = cFirstMatr + 1;
				
				}
				
				cFirstMatr = 0;
				cSecondMatr = cSecondMatr + 1;	
			}
			
			cSecondMatr = 0;
			r = r + 1;
		}

		return 0;
	}
	
	def printMatrix(m : Int[], r : Int, c : Int) : Int = {
		
		var rbis : Int;
		var cbis : Int;
		var index : Int;
		var result : String;
		
		rbis = 0;
		cbis = 0;
		result = "";
		
		println("Matrix " + r + " by " + c);
		
		while(rbis < r) {
		
			while(cbis < c) {
				
				index = rbis * c + cbis;
				result = result + m[index] + " ";
				
				cbis = cbis + 1;
			}
			
			println(result);
			
			cbis = 0;
			rbis = rbis + 1;
			result = "";
			
		}
		
		println("");
		
		return 0;
	}
	
	def printAll() : Int = {
		
		println("Matrix 1");
		do(this.printMatrix(m1, row1, col1));
		
		println("Matrix 2");
		do(this.printMatrix(m2, row2, col2));
		
		println("Result of the multiplication");
		do(this.printMatrix(m3, row3, col3));
	
		return 0;
		
	}

}

