package com.r4intellij.lang;

import com.intellij.CommonBundle;
import org.jetbrains.annotations.PropertyKey;

import java.util.ResourceBundle;

/**
 * Created by moon on 7/23/14.
 */
public class RBundle {
	private static final String BUNDLE_NAME = "messages.RBundle";
	private static final ResourceBundle BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	public static String message(@PropertyKey(resourceBundle = BUNDLE_NAME) String key, Object... params) {
		return CommonBundle.message(BUNDLE, key, params);
	}
}
