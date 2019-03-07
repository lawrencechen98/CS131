import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) { 
    	value = new AtomicIntegerArray(ByteArrToIntArr(v)); 
    	maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) { 
    	value = new AtomicIntegerArray(ByteArrToIntArr(v)); 
    	maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() { return AtomIntArrToByteArr(value); }

    public boolean swap(int i, int j) {
		int ival = value.get(i);
		int jval = value.get(j);
		if (ival <= 0 || jval >= maxval) {
		    return false;
		}
		value.set(i, ival+1);
		value.set(j, jval+1);
		return true;
    }

    private int[] ByteArrToIntArr(byte[] byteArr) {
    	int[] intArr = new int[byteArr.length];
    	for (int i = 0; i < byteArr.length; intArr[i] = byteArr[i++]);
    	return intArr;
    }

    private byte[] AtomIntArrToByteArr(AtomicIntegerArray atomIntArr) {
    	int len = atomIntArr.length();
    	byte[] byteArr = new byte[len];
    	for (int i = 0; i < len; byteArr[i] = (byte) atomIntArr.get(i++));
    	return byteArr;
    }
}