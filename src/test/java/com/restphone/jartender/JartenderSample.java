package com.restphone.jartender;

import java.io.File;
import java.io.IOException;

@AnnotationI(a = "i", b = @AnnotationII)
public class JartenderSample implements InterfaceI {
	@AnnotationI(a = "i", b = @AnnotationII)
	class JartenderSampleSubclass {
		public JartenderSample methodInSubclass() {
			return null;
		};
	}

	@AnnotationI(a = "i", b = @AnnotationII)
	public JartenderSample[] testClassMethod(
			@AnnotationI(a = "i", b = @AnnotationII) String s) {
		@AnnotationI(a = "ii", b = @AnnotationII)
		Class<JartenderSample> x = JartenderSample.class;
		System.out.println(x);

		// Use a static field
		System.out.println(JartenderSampleII.aStaticStringFieldWithAnnotation);

		JartenderSampleII j = new JartenderSampleII();
		System.out.println(j.aFieldWithAnnotation + j.aGenericMethod("j"));
		
		return null;
	}
	
	public void canRaiseException() throws RuntimeException {
		try {
			File f = File.createTempFile("", "");
			System.out.println(f);
		} catch (IOException e) {
		} catch (RuntimeException e) {
		}
		throw new RuntimeException();
	}

	@AnnotationI(a = "ii", b = @AnnotationII)
	public static String aStaticStringFieldWithAnnotation = "f";
	@AnnotationI(a = "ii", b = @AnnotationII)
	public String aFieldWithAnnotation = "ff";

	public static String aStaticStringFieldWithoutAnnotation = "fff";
	public String aFieldWithoutAnnotation = "ffff";

	public <T> String aGenericMethod(T x) {
		return "s";
	}
}
