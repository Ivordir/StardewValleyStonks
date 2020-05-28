using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Configuration;

namespace StardewValleyStonks
{
    public static class Extentions
    {
        public static string WrapTag(this string value, string tag)
        => $"<{tag}>{value}</{tag}>";

        public static int[] ToArray(this IConfigurationSection config)
        => config.ToArray(s => Convert.ToInt32(s));
        public static T[] ToArray<T>(
            this IConfigurationSection config,
            Func<string, T> converter)
        => config.ConvertAll(converter).ToArray();
        public static IEnumerable<T> ConvertAll<T>(
            this IConfigurationSection config,
            Func<string, T> converter)
        => config.GetChildren().Select(s => converter(s.Value));
        public static IEnumerable<string> RawValues(this IConfigurationSection config)
        => config.GetChildren().Select(s => s.Value);
    }

    //assumptions list:
    //mutliProcesses always create an output item with normal quality
    //a higher quality item always sells for more than a lower quality item,
    //  and putting a higher quality item into, for example, a keg
    //  will always result in a product that sells for more than or equal to
    //  a product made from a lower quality item.
}
