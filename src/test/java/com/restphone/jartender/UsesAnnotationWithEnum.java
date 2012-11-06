package com.restphone.jartender;

public @interface UsesAnnotationWithEnum {
	AnnotationEnum[] enumArray();
	AnnotationEnum enumSingle();
}

 enum AnnotationEnum {
	SAMPLEVALUE1, SAMPLEVALUE2;
}
