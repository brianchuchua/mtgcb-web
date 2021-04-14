import { Dispatch, SetStateAction, useState } from 'react';
import styled from 'styled-components';
import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import CardGalleryControls from './CardGalleryControls';
import CardBox, { Card } from './CardBox';

interface CardGalleryProps {
  cards: Card[];
  cardsPerRow?: number;
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
}

const CardGallery: React.FC<CardGalleryProps> = ({ cards, first, skip, page, totalResults, setSkip, setFirst, setPage }) => {
  const [cardsPerRow, setCardsPerRow] = useState(4);
  const [galleryWidth, setGalleryWidth] = useState(100);
  const atLeastOneCardToShow = totalResults > 0;

  return atLeastOneCardToShow ? (
    <>
      <CardGalleryControls
        cards={cards}
        first={first}
        page={page}
        setCardsPerRow={setCardsPerRow}
        setFirst={setFirst}
        setGalleryWidth={setGalleryWidth}
        setPage={setPage}
        setSkip={setSkip}
        skip={skip}
        totalResults={totalResults}
        cardsPerRow={cardsPerRow}
      />

      <CardGalleryWrapper cardsPerRow={cardsPerRow} galleryWidth={galleryWidth}>
        {cards.map((card) => (
          <CardBox key={card.id} card={card} />
        ))}
      </CardGalleryWrapper>
    </>
  ) : (
    <Grid container alignItems="center" justify="center">
      <Grid item>
        <Typography variant="h6">No results found -- try another search!</Typography>
      </Grid>
    </Grid>
  );
};

interface CardGalleryWrapperProps {
  cardsPerRow: number;
  galleryWidth: number;
}

const CardGalleryWrapper = styled.div<CardGalleryWrapperProps>(({ cardsPerRow = 4, galleryWidth = 100 }) => ({
  display: 'grid',
  gridTemplateColumns: `repeat(${cardsPerRow}, minmax(0, 1fr))`,
  gridTemplateRows: 'repeat(3, 1fr)',
  gap: `5px 10px`,
  width: `${galleryWidth}%`,
  margin: '0 auto',
}));

export default CardGallery;
