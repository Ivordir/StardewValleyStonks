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
    public int Index { get; set; }

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

