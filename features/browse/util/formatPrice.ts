import { PriceTypes } from '../browseSlice';

export interface Card {
  id: number;
  name: string;
  set: {
    name: string;
  };
  low: number;
  average: number;
  high: number;
  market: number;
  foil: number;
  tcgplayerId: number;
  priceId: {
    low: number;
    average: number;
    high: number;
    market: number;
    foil: number;
  };
}

export const formatter = new Intl.NumberFormat('en-US', {
  style: 'currency',
  currency: 'USD',
  minimumFractionDigits: 2,
});

export const formatPrice = (card: Card, priceType: PriceTypes, includeFoilAnnotation = true) => {
  const price = card?.[priceType] || card?.priceId?.[priceType] || 0;
  const foilPrice = card?.foil || card?.priceId?.foil || 0;
  const isFoil = priceType === 'foil';

  if (price === 0 && isFoil) {
    return '';
  }
  if (price === 0 && !isFoil && foilPrice !== 0) {
    return '';
  }
  if (price === 0 && !isFoil && foilPrice === 0) {
    return 'N/A';
  }

  const formattedPrice = formatter.format(price);

  return isFoil && includeFoilAnnotation ? ` (${formattedPrice} foil)` : formattedPrice;
};
