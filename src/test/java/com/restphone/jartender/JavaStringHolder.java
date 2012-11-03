package com.restphone.jartender;


public class JavaStringHolder {
	public static String jartenderSample = "public class JartenderSample implements InterfaceI {\r\n" + 
			"	class JartenderSampleSubclass {\r\n" + 
			"		\r\n" + 
			"	}\r\n" + 
			"	\r\n" + 
			"	@AnnotationI(a = \"i\", b = @AnnotationII)\r\n" + 
			"	public void testClassMethod() {\r\n" + 
			"		@AnnotationI(a = \"ii\", b = @AnnotationII)\r\n" + 
			"		Class<JartenderSample> x = JartenderSample.class;\r\n" + 
			"		System.out.println(x);\r\n" + 
			"		\r\n" + 
			"		// Use a static field\r\n" + 
			"		System.out.println(JartenderSampleII.aStaticStringFieldWithAnnotation);\r\n" + 
			"		\r\n" + 
			"		JartenderSampleII j = new JartenderSampleII();\r\n" + 
			"		System.out.println(j.aFieldWithAnnotation + j.aGenericMethod(\"j\"));\r\n" + 
			"	}\r\n" + 
			"\r\n" + 
			"	@AnnotationI(a = \"ii\", b = @AnnotationII)\r\n" + 
			"	public static String aStaticStringFieldWithAnnotation = \"f\";\r\n" + 
			"	@AnnotationI(a = \"ii\", b = @AnnotationII)\r\n" + 
			"	public String aFieldWithAnnotation = \"ff\";\r\n" + 
			"\r\n" + 
			"	public static String aStaticStringFieldWithoutAnnotation = \"fff\";\r\n" + 
			"	public String aFieldWithoutAnnotation = \"ffff\";\r\n" + 
			"	\r\n" + 
			"	public <T> String aGenericMethod(T x) {\r\n" + 
			"		return \"s\";\r\n" + 
			"	}\r\n" + 
			"}\r\n";
}
