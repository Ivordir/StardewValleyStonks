using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface INullComparable<T>
    {
        public int? CompareTo(T other);
    }

    public static class NullCompareExtentions
    {
        public static LinkedList<T> DoComparisons<T>(this LinkedList<T> list, Dictionary<T, List<T>> equalDict)
            where T : INullComparable<T>
        {
            LinkedListNode<T> item = list.First;
            while (item != list.Last)
            {
                LinkedListNode<T> other = item.Next;
                while (other != null)
                {
                    int? comparisson = item.Value.CompareTo(other.Value);
                    if (comparisson == 0)
                    {
                        if (equalDict.ContainsKey(item.Value))
                        {
                            equalDict[item.Value].Add(other.Value);
                        }
                        else
                        {
                            equalDict.Add(item.Value, new List<T> { other.Value });
                        }
                        list.Remove(other);
                    }
                    else if (comparisson == 1)
                    {
                        list.Remove(other);
                    }
                    else if (comparisson == -1)
                    {
                        list.Remove(item);
                        break;
                    }
                    other = other.Next;
                }
                item = item.Next;
            }
            return list;
        } 
    }
}
