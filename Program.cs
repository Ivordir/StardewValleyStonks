using System.Threading.Tasks;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.DependencyInjection;

namespace StardewValleyStonks
{
    public class Program
    {
        public static async Task Main(string[] args)
        {
            var builder = WebAssemblyHostBuilder.CreateDefault(args);
            builder.RootComponents.Add<App>("app");
            builder.Services.AddScoped<DateState>();
            builder.Services.AddScoped<SkillsState>();
            builder.Services.AddScoped<SettingsState>();
            builder.Services.AddScoped<OutputState>();
            //builder.Services.AddBaseAddressHttpClient();
            await builder.Build().RunAsync();
        }
    }
}
