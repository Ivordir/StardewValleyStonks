using System.Threading.Tasks;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.Extensions.DependencyInjection;
using BlazorStyled;

namespace StardewValleyStonks
{
    public class Program
    {
        public static async Task Main(string[] args)
        {
            WebAssemblyHostBuilder builder = WebAssemblyHostBuilder.CreateDefault(args);
            builder.Services.AddBlazorStyled();

            builder.RootComponents.Add<App>("app");
            builder.Services.AddScoped<Date>();
            builder.Services.AddScoped<Skills>();
            builder.Services.AddScoped<Output>();
            builder.Services.AddScoped<Settings>();
            builder.Services.AddScoped<Data>();

            var host = builder.Build();
            await host.RunAsync();
        }
    }
}
