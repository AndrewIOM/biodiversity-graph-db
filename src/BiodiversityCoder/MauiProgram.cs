using Microsoft.AspNetCore.Components.WebView.Maui;
using BiodiversityCoder.Core;

namespace BiodiversityCoder;

public static class MauiProgram
{
	public static MauiApp CreateMauiApp()
	{
		var builder = MauiApp.CreateBuilder();
		builder
			.UseMauiApp<App>()
			.ConfigureFonts(fonts =>
			{
				fonts.AddFont("OpenSans-Regular.ttf", "OpenSansRegular");
			});

		#if WINDOWS
				builder.Services.AddTransient<IFolderPicker, Platforms.Windows.FolderPicker>();
		#elif MACCATALYST
				builder.Services.AddTransient<IFolderPicker, Platforms.MacCatalyst.FolderPicker>();
		#endif
		
		builder.Services.AddMauiBlazorWebView();
		
		return builder.Build();
	}
}
