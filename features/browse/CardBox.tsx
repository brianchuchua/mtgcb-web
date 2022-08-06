import Grid from '@material-ui/core/Grid';
import Skeleton from '@material-ui/lab/Skeleton';
import { useRef, useState } from 'react';
import LazyLoad from 'react-lazyload';
import styled from 'styled-components';
import Link from '../../components/Link';
import { useContainerDimensions } from '../../util/useContainerDimensions';
import { PriceTypes } from './browseSlice';
import CardPrice from './CardPrice';
import CardQuantity from './CardQuantity';
import CardQuantitySelector from './CardQuantitySelector';
import { Card } from './types/Card';

interface CardBoxProps {
  card: Card;
  priceType: PriceTypes;
  nameIsVisible?: boolean;
  priceIsVisible?: boolean;
  setIsVisible?: boolean;
  userId?: string;
  loggedInUserId?: string;
  quantityReg?: number;
  quantityFoil?: number;
  fixedHeight?: string;
}

const CardBox: React.FC<CardBoxProps> = ({
  card,
  priceType,
  nameIsVisible = true,
  priceIsVisible = true,
  setIsVisible = true,
  userId = null,
  loggedInUserId = null,
  quantityReg = null,
  quantityFoil = null,
  fixedHeight = null,
}) => {
  const imageUrl = `https://mtgcb-images.s3.amazonaws.com/cards/images/normal/${card.id}.jpg`;

  const [imageLoaded, setImageLoaded] = useState(false);
  const componentRef = useRef();
  const { width, height } = useContainerDimensions(componentRef);

  const computedHeight = Math.ceil((680 / 488) * width);
  return (
    <CardWrapper key={card.id} fixedHeight={fixedHeight}>
      <CardAttributes>
        <div ref={componentRef}>
          <LazyLoad key={`lazy-${card.id}`} once resize style={{ width: '100%' }}>
            {!imageLoaded && (
              <Skeleton
                variant="rect"
                width="100%"
                height={fixedHeight || `${computedHeight}px`}
                animation="wave"
                style={{
                  width: fixedHeight ? Math.ceil((488 / 680) * Number(fixedHeight.replace('px', ''))) : '100%',
                  height: fixedHeight || `${computedHeight}px`,
                  borderRadius: '5%',
                  padding: '0',
                  margin: '0',
                }}
              />
            )}
            <a href={generateCardUrl(card.tcgplayerId, card.name)} target="_blank" rel="noreferrer">
              <CardImage
                alt={card.name}
                title={card.name}
                src={imageUrl}
                set={card.set?.name}
                fixedHeight={fixedHeight}
                computedHeight={computedHeight}
                style={{ display: imageLoaded ? 'block' : 'none' }}
                onLoad={() => setImageLoaded(true)}
              />
            </a>
          </LazyLoad>
        </div>
        {userId && userId === loggedInUserId && (
          <Grid container spacing={1}>
            <Grid item xs={6}>
              <CardQuantitySelector
                cardId={card.id}
                cardName={card.name}
                quantityReg={quantityReg}
                userId={userId}
                setId={card.set?.id}
                renderFoil={false}
              />
            </Grid>
            <Grid item xs={6}>
              <CardQuantitySelector
                cardId={card.id}
                cardName={card.name}
                quantityFoil={quantityFoil}
                userId={userId}
                setId={card.set?.id}
                renderNormal={false}
              />
            </Grid>
          </Grid>
        )}
        {nameIsVisible && <CardName title={card.name}>{card.name}</CardName>}
        {setIsVisible && (
          <Link href={userId ? `/collections/${userId}/sets/${card.set?.slug}` : `/browse/sets/${card.set?.slug}`}>
            <CardSet title={card.set?.name}>{card.set?.name ?? 'Unknown Set'}</CardSet>
          </Link>
        )}
        {userId && userId !== loggedInUserId && <CardQuantity quantityReg={quantityReg} quantityFoil={quantityFoil} />}
        {priceIsVisible && <CardPrice card={card} priceType={priceType} />}
      </CardAttributes>
    </CardWrapper>
  );
};

const generateCardUrl = (cardId: string | number, cardName) =>
  cardId
    ? `https://shop.tcgplayer.com/magic/product/productsearch?id=${cardId}&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR`
    : `https://www.tcgplayer.com/search/magic/product?productLineName=magic&q=${cardName}&utm_campaign=affiliate&utm_medium=CTNBLDR&utm_source=CTNBLDR`;

interface CardWrapperProps {
  fixedHeight?: string;
}

const CardWrapper = styled.div<CardWrapperProps>(({ fixedHeight }) => ({
  display: 'inline-block',
  fontSize: 'clamp(16px, 1.0vw, 22px)',
  height: fixedHeight || 'auto',
  minHeight: '50px',
  lineHeight: 1.43,
}));

const CardAttributes = styled.div({ textAlign: 'center', maxWidth: '100%' });

const CardName = styled.div({ whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis', maxWidth: '100%' });

const CardSet = styled.em(({ theme }) => ({
  display: 'block',
  whiteSpace: 'nowrap',
  overflow: 'hidden',
  textOverflow: 'ellipsis',
  maxWidth: '100%',
}));

interface CardImageProps {
  set?: string;
  fixedHeight?: string;
  computedHeight?: number;
}

const CardImage = styled.img<CardImageProps>(({ set, fixedHeight, computedHeight }) => ({
  width: fixedHeight ? 'auto' : '100%',
  height: fixedHeight || `${computedHeight}px`,
  borderRadius: set === 'Limited Edition Alpha' ? '7%' : '5%',
}));

export default CardBox;
