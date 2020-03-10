using System;
using System.Collections.Generic;

public class Source : IComparable
{
    public static Source None;
    public static List<Source> Sources;
    private static int LastIndex;

    static Source()
    {
        LastIndex = -1;
        None = new Source("Free", false);
        Sources = new List<Source>();
    }

    public string Name { get; }
    public bool Enabled { get; } //should this source be enabled by default for all items
    public int Index  //for consistency, what order does this source appear in a list
    {
        get
        {
            return Index;
        }
        set
        {
            if (value != Index)
            {
                return;
            }
            else if (value < Index)
            {
                for (int i = 0; i < Sources.Count; i++)
                {
                    if (Sources[i].Index < Index && Sources[i].Index >= value)
                    {
                        Sources[i].Index++;
                    }
                }
            }
            else //if (value > Index)
            {
                for (int i = 0; i < Sources.Count; i++)
                {
                    if (Sources[i].Index > Index && Sources[i].Index <= value)
                    {
                        Sources[i].Index--;
                    }
                }
            }
            Sources.RemoveAt(Index);
            Index = value;
            Sources.Insert(Index, this);
        }
    }

    public Source (string name, bool enabled)
    {
        Name = name;
        Enabled = enabled;
        Index = LastIndex;
        LastIndex++;
        Sources.Add(this);
    }

    public int CompareTo(object obj)
    {
        return Index.CompareTo(((Source)obj).Index);
    }

    public override int GetHashCode()
    {
        return Name.GetHashCode();
    }
}

