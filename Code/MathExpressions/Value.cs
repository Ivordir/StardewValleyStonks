namespace StardewValleyStonks
{
    public struct Value
    {
        public static implicit operator double(Value value) => value.V.Value;

        private IValue V { get; }

        public Value(IValue value)
        {
            V = value;
        }
    }
}
