using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class Source : IComparable<Source>
    {
        public static Source None;
        public static List<Source> Sources;
        private static int LastIndex;

        static Source()
        {
            LastIndex = 0;
            None = new Source(false); //read default enabled value for Free Source from config
            Sources = new List<Source> { None };
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

        public List<ICondition> Conditions { get; }
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