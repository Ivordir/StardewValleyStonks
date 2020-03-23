using System;

namespace StardewValleyStonks.Objects.Source
{
    public interface ISource : IComparable<ISource>
    {
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
        public int CompareTo(ISource source)
        {
            return Index.CompareTo(source.Index);
        }
    }
}
