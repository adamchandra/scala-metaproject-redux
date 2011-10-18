/* Copyright (C) 2004 Univ. of Massachusetts Amherst, Computer Science Dept.
 * Based on MD5Hash.java from Nutch
 * author: mikem
 * */

/* Copyright (c) 2003 The Nutch Organization.  All rights reserved.   */
/* Use subject to the conditions in http://www.nutch.org/LICENSE.txt. */

package cc.acs.commons.util;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.UUID;

public class SHA1Hash {

	public static final int SHA1_LEN = 20;
	private static final MessageDigest DIGESTER;

	static {
		try {
			DIGESTER = MessageDigest.getInstance( "SHA1" );
		}
		catch (NoSuchAlgorithmException e) {
			throw new RuntimeException( e );
		}
	}

	private byte _digest[];

	/**
	 * Constructs an SHA1Hash.
	 */
	public SHA1Hash() {
		_digest = new byte[SHA1_LEN];
	}

	public static SHA1Hash createFromHexString(String hexString) {
		try {
			SHA1Hash sha1Hash = new SHA1Hash( hexString );
			return sha1Hash;
		}
		catch (IllegalArgumentException e) {
			throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage());
		}
	}

	/**
	 * Constructs an SHA1Hash from a hex string.
	 */
	public SHA1Hash(String hex) {
		setDigest( hex );
	}
	
	/**
	 * Creates a globally unique hash (using the Java5 UUID class)
	 * @return
	 */
	public static SHA1Hash createUnique() {
	  UUID uuid = UUID.randomUUID();
	  return SHA1Hash.digest(uuid.toString());
	}

	/**
	 * Constructs an SHA1Hash with a specified value.
	 */
	public SHA1Hash(byte digest[]) {
		if (digest.length != SHA1_LEN) {
			throw new IllegalArgumentException( "Wrong length: " + digest.length );
		}
		this._digest = digest;
	}

	/**
	 * Copy the contents of another instance into this instance.
	 */
	public void set(SHA1Hash that) {
		System.arraycopy( that._digest, 0, _digest, 0, SHA1_LEN );
	}

	/**
	 * Returns the digest bytes.
	 */
	public byte[] getDigest() {
		return _digest;
	}

	/**
	 * Construct a hash value for a byte array.
	 */
	public static SHA1Hash digest(byte[] data) {
		return digest( data, 0, data.length );
	}

	/**
	 * Construct a hash value for a byte array.
	 */
	public static SHA1Hash digest(byte[] data, int start, int len) {
		byte digest[];
		synchronized (DIGESTER) {
			DIGESTER.update( data, start, len );
			digest = DIGESTER.digest();
		}
		return new SHA1Hash( digest );
	}

	/**
	 * Construct a hash value for a File object.
	 */
	public static SHA1Hash digest(File file) throws IOException {
		byte digest[];
		InputStream in = new BufferedInputStream( new FileInputStream( file ) );
		synchronized (DIGESTER) {
			byte data[] = new byte[8192];
			int len = in.read( data );
			while (len > 0) {
				DIGESTER.update( data, 0, len );
				len = in.read( data );
			}
			digest = DIGESTER.digest();
		}
		in.close();
		return new SHA1Hash( digest );
	}

	/**
	 * Construct a hash value for a String.
	 */
	public static SHA1Hash digest(String string) {
		return digest( string.getBytes() );
	}

	/**
	 * Returns true iff <code>o</code> is an SHA1Hash whose digest contains the same values.
	 */
	@Override
  public boolean equals(Object o) {
		if (!(o instanceof SHA1Hash)) {
			return false;
		}
		SHA1Hash other = (SHA1Hash)o;
		return Arrays.equals( _digest, other._digest );
	}

	/**
	 * Returns a hash code value for this object.
	 */
	@Override
  public int hashCode() {
		return (_digest[0] | (_digest[1] << 8) | (_digest[2] << 16) | (_digest[3] << 24)) ^
			   (_digest[4] | (_digest[5] << 8) | (_digest[6] << 16) | (_digest[7] << 24)) ^
			   (_digest[8] | (_digest[9] << 8) | (_digest[10] << 16) | (_digest[11] << 24)) ^
			   (_digest[12] | (_digest[13] << 8) | (_digest[14] << 16) | (_digest[15] << 24)) ^
			   (_digest[16] | (_digest[17] << 8) | (_digest[18] << 16) | (_digest[19] << 24));
	}

	public static String sha1(String s) {
		return SHA1Hash.digest(s).toString();
	}
	
	/**
	 * Returns a string representation of this object.
	 */
	@Override
  public String toString() {
		StringBuffer buffer = new StringBuffer( SHA1_LEN * 2 );
		for (int i = 0; i < SHA1_LEN; i++) {
			HexUtils.append( buffer, _digest[i] );
		}
		return buffer.toString();
	}

	/**
	 * Sets the digest value from a hex string.
	 */
	public void setDigest(String hex) {
		if (hex.length() != SHA1_LEN * 2) {
			throw new IllegalArgumentException( "Wrong length: " + hex.length() );
		}
		byte digest[] = new byte[SHA1_LEN];
		for (int i = 0; i < SHA1_LEN; i++) {
			int j = i << 1;
			digest[i] = HexUtils.toByte( hex, j );
		}
		this._digest = digest;
	}
	/**
	 * Return true if given string is a valid SHA1-hash.
	 * 
	 * @param s
	 * @return
	 */
	static public boolean isHash(String s) {
		if (s == null || s.length() != SHA1_LEN*2) { return false; }
		for (int i = 0; i < SHA1_LEN*2; i++) {
			char c = s.charAt(i);
			if ((c >= '0' && c <= '9') ||
				(c >= 'a' && c <= 'f') ||
				(c >= 'A' && c <= 'F')) { continue; }
			return false;
		}
		return true;
	}
}
