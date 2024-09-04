using BiodiversityCoder.Core;
using CommunityToolkit.Maui;
using CommunityToolkit.Maui.Storage;

namespace BiodiversityCoder;

public static class MauiProgram
{
	public static MauiApp CreateMauiApp()
	{
		var builder = MauiApp.CreateBuilder();
		builder
			.UseMauiApp<App>()
			.UseMauiCommunityToolkit()
			.ConfigureFonts(fonts =>
			{
				fonts.AddFont("OpenSans-Regular.ttf", "OpenSansRegular");
			});
		
		builder.Services.AddTransient<BiodiversityCoder.Core.IFolderPicker, CrossPlatformFolderPicker>();
		builder.Services.AddMauiBlazorWebView();
		
		#if DEBUG
			builder.Services.AddBlazorWebViewDeveloperTools();
		#endif

		return builder.Build();
	}
}

public class CrossPlatformFolderPicker() : BiodiversityCoder.Core.IFolderPicker
{
    public async Task<string> PickFolder(CancellationToken cancellationToken)
    {
        var result = await FolderPicker.Default.PickAsync(cancellationToken);
        result.EnsureSuccess();
		return result.Folder.Path;
    }
}
