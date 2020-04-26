using System.Threading.Tasks;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace StardewValleyStonks
{
    public class Program
    {
        public static async Task Main(string[] args)
        {
            WebAssemblyHostBuilder builder = WebAssemblyHostBuilder.CreateDefault(args);
            //builder.Configuration.AddJsonFile("appsettings.json", false, false);
            //builder.Services.AddTransient(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
            builder.Services.AddScoped<DateState>();
            builder.Services.AddScoped<SkillsState>();
            builder.Services.AddScoped<OutputState>();
            builder.RootComponents.Add<App>("app");

            var host = builder.Build();

            builder.Services.Configure<SettingsState>(settings =>
                new SettingsState(host.Configuration.GetSection("test").GetValue<bool>("test"))
            ) ;

            await host.RunAsync();
        }
    }
}
