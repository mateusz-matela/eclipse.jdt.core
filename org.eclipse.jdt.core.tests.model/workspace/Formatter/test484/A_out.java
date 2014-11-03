public class A {
	public void launch() {
		try {
			if ((javaProject == null) || !javaProject.exists()) {
				abort(PDEPlugin.getResourceString(
						"JUnitLaunchConfiguration.error.invalidproject"), //$NON-NLS-1$
						null,
						IJavaLaunchConfigurationConstants.ERR_NOT_A_JAVA_PROJECT);
			}
		} catch (CoreException e) {
		}
	}
}