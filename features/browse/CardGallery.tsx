import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import { Dispatch, SetStateAction, useState } from 'react';
import styled from 'styled-components';
import { PriceTypes } from './browseSlice';
import CardBox, { Card } from './CardBox';
import CardGalleryControls from './CardGalleryControls';

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
  priceType: PriceTypes;
}

const CardGallery: React.FC<CardGalleryProps> = ({ cards, first, skip, page, totalResults, setSkip, setFirst, setPage, priceType }) => {
  const [cardsPerRow, setCardsPerRow] = useState(4);
  const [galleryWidth, setGalleryWidth] = useState(100);
  const [nameIsVisible, setNameIsVisible] = useState(true);
  const [setIsVisible, setSetIsVisible] = useState(true);
  const [priceIsVisible, setPriceIsVisible] = useState(true);

  const atLeastOneCardToShow = totalResults > 0;

  const settingGroups = [
    {
      label: 'Show Fields',
      type: 'toggleFilters',
      settings: [
        {
          key: 'name',
          label: 'Name',
          isVisible: nameIsVisible,
          setVisibility: setNameIsVisible,
        },
        {
          key: 'set',
          label: 'Set',
          isVisible: setIsVisible,
          setVisibility: setSetIsVisible,
        },
        {
          key: 'price',
          label: 'Price',
          isVisible: priceIsVisible,
          setVisibility: setPriceIsVisible,
        },
      ],
    },
  ];

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
        settingGroups={settingGroups}
      />

      <CardGalleryWrapper cardsPerRow={cardsPerRow} galleryWidth={galleryWidth}>
        {cards &&
          cards.map((card) => (
            <CardBox
              key={card.id}
              card={card}
              priceType={priceType}
              nameIsVisible={nameIsVisible}
              setIsVisible={setIsVisible}
              priceIsVisible={priceIsVisible}
            />
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
