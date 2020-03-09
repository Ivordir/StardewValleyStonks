using System;
using System.Collections.Generic;

public class Source : IComparable
{
    public static Source None;
    public static List<Source> Sources;
    private static int LastIndex;

    static Source()
    {
        LastIndex = 0;
        None = new Source("Free", false);
        LastIndex = 0;
        Sources = new List<Source> { None };
    }

    public string Name { get; }
    public bool Enabled { get; }
    public int Index { get; set; }

    public Source (string name, bool enabled)
    {
        Name = name;
        Enabled = enabled;
        Index = LastIndex;
        LastIndex++;
        None.Index = LastIndex;
        Sources.Add(this);
    }

    public int CompareTo(object obj)
    {
        return Index.CompareTo(((Source)obj).Index);
    }

    public override int GetHashCode()
    {
        return Index.GetHashCode();
    }
}

