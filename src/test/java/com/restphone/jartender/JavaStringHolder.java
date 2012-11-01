package com.restphone.jartender;

public class JavaStringHolder {
	public static String simpleAnnotation = "package com.restphone.asmSample;\r\n" + 
			"\r\n" + 
			"import java.lang.annotation.ElementType;\r\n" + 
			"import java.lang.annotation.Target;\r\n" + 
			"\r\n" + 
			"@Target(ElementType.METHOD)\r\n" + 
			"public @interface SimpleAnnotation {\r\n" + 
			"    @SecondAnnotation public String a();\r\n" + 
			"    public SecondAnnotation b();\r\n" + 
			"}\r\n";
}
