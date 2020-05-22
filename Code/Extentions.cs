using System;
using System.Collections.Concurrent;
using System.Text;

namespace StardewValleyStonks
{
    public static class Extentions
    {
        public static Func<X, T> Memoize<X, T>(this Func<X, T> f)
        {
            ConcurrentDictionary<X, T> cache = new ConcurrentDictionary<X, T>();
            return x => cache.GetOrAdd(x, f(x));
        }

        public static Func<X, Y, T> Memoize<X, Y, T>(this Func<X, Y, T> f)
        {
            ConcurrentDictionary<(X, Y), T> cache = new ConcurrentDictionary<(X, Y), T>();
            return (x, y) => cache.GetOrAdd((x ,y), f(x, y));
        }

        public static Func<X, Y, Z, T> Memoize<X, Y, Z, T>(this Func<X, Y, Z, T> f)
        {
            ConcurrentDictionary<(X, Y, Z), T> cache = new ConcurrentDictionary<(X, Y, Z), T>();
            return (x, y, z) => cache.GetOrAdd((x, y, z), f(x, y, z));
        }

        public static int Price(this BestDict<Source, BuyPrice> bestFinder)
        {
            return bestFinder.HasBestItem ? bestFinder.BestItems[0].Price : 0;
        }

        public static int InRange(this int value, int min, int max)
        {
            if (value < min)
            {
                return min;
            }
            else if (value > max)
            {
                return max;
            }
            return value;
        }

        public static int WithMin(this int value, int min)
        {
            if (value < min)
            {
                return min;
            }
            return value;
        }

        public static int WithMax(this int value, int max)
        {
            if (value > max)
            {
                return max;
            }
            return value;
        }

        public static double InRange(this double value, int min, int max)
        {
            if (value < min)
            {
                return min;
            }
            else if (value > max)
            {
                return max;
            }
            return value;
        }

        public static double WithMin(this double value, int min)
        {
            if (value < min)
            {
                return min;
            }
            return min;
        }
        public static double WithMax(this double value, int max)
        {
            if (value > max)
            {
                return max;
            }
            return value;
        }

        public static string Repeat(this string str, int num)
        {
            string repeat = "";
            for (int i = 0; i < num; i++)
            {
                repeat += str;
            }
            return repeat;
        }
    }
}
