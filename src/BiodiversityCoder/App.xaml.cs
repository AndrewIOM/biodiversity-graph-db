using Microsoft.AspNetCore.Components.WebView.Maui;

namespace BiodiversityCoder;

public partial class App : Application
{

	public static ContentPage GetMainPage()
	{
		var comp = new RootComponent
		{
			Selector = "#app",
			ComponentType = typeof(BiodiversityCoder.Core.MainApp)
		};
		var webview = new BlazorWebView
		{
			HostPage = "wwwroot/index.html"
		};
		webview.RootComponents.Add(comp);
		return new ContentPage
		{
			Content = webview,
		};
	}
	
    protected override Window CreateWindow(IActivationState? activationState)
    {
        return new Window(GetMainPage());
    }
}
