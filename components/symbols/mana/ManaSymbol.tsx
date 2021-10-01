const ManaSymbol: React.FC<ManaSymbolProps> = ({ size = 3, symbol = 'c', margin = 0, half = false }) => (
  <i className={`ms ms-${symbol} ${half ? 'ms-half' : ''} ms-cost ms-${size}x`} style={{ margin: `0 ${margin}px 0 ${margin}px` }} />
);
export interface ManaSymbolProps {
  size: number;
  margin?: number;
  half?: boolean;
  symbol?: string;
}

export default ManaSymbol;
