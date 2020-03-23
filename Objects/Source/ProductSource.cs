using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class ProductSource : Source
    {
        public static ProductSource None;
        public static List<ProductSource> Sources;
        private static int LastIndex;

        static ProductSource()
        {
            LastIndex = 0;
            None = new ProductSource(false); //read default enabled value for Free Source from config
            Sources = new List<ProductSource> { None };
        }

        public string Name { get; }
        public bool IsEnabled { get; set; } //should this source be enabled for all items (in lite input mode)?
        public int Index  //for consistency, what order does this source appear in a list
        {
            get
            {
                return Index;
            }
            set
            {
                if (value < Index)
                {
                    for (int i = 0; i < Sources.Count; i++)
                    {
                        if (Sources[i].Index < Index && Sources[i].Index >= value)
                        {
                            Sources[i].Index++;
                        }
                    }
                }
                else if (value > Index)
                {
                    for (int i = 0; i < Sources.Count; i++)
                    {
                        if (Sources[i].Index > Index && Sources[i].Index <= value)
                        {
                            Sources[i].Index--;
                        }
                    }
                }
                else //value == Index
                {
                    return;
                }
                Sources.RemoveAt(Index);
                Index = value;
                Sources.Insert(Index, this);
            }
        }

        public Source(string name, bool enabled)
        {
            Name = name;
            IsEnabled = enabled;
            Index = LastIndex;
            LastIndex++;
            Sources.Insert(Index, this);
        }

        private Source(bool enabled)
        {
            Name = "Free";
            IsEnabled = enabled;
            Index = int.MaxValue;
        }

        public override int GetHashCode()
        {
            return Name.ToLower().GetHashCode();
        }

        public int CompareTo(Source source)
        {
            return Index.CompareTo(source.Index);
        }
    }
}
