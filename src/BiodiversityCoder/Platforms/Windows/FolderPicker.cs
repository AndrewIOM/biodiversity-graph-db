using WindowsFolderPicker = Windows.Storage.Pickers.FolderPicker;
using BiodiversityCoder.Core;

namespace BiodiversityCoder.Platforms.Windows
{
    public class FolderPicker : IFolderPicker
    {
        public async Task<string> PickFolder()
        {
            var folderPicker = new WindowsFolderPicker();
            var hwnd = ((MauiWinUIWindow)App.Current.Windows[0].Handler.PlatformView).WindowHandle;
            WinRT.Interop.InitializeWithWindow.Initialize(folderPicker, hwnd);
            var result = await folderPicker.PickSingleFolderAsync();
            return result.Path;
        }
    }
}