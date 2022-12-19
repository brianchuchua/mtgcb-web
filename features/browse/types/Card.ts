export interface Card {
  id: number;
  name: string;
  set: {
    id: string;
    name: string;
    slug: string;
  };
  low: number;
  average: number;
  high: number;
  market: number;
  foil: number;
  tcgplayerId: number;
  priceId?: {
    low: number;
    average: number;
    high: number;
    market: number;
    foil: number;
  };
}
