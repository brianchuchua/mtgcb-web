import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
import Skeleton from '@material-ui/lab/Skeleton';
import { Dispatch, SetStateAction, useState } from 'react';
import styled from 'styled-components';
import { PriceTypes } from './browseSlice';
import GalleryControls from './GalleryControls';
import SetBox, { Set, SetSummary } from './SetBox';

const SetGallery: React.FC<SetGalleryProps> = ({
  sets,
  costsToPurchase,
  first,
  skip,
  page,
  totalResults,
  setSkip,
  setFirst,
  setPage,
  priceType,
  userId = null,
  isLoading,
  isFetching,
}) => {
  const [setsPerRow, setSetsPerRow] = useState(4);
  const [galleryWidth, setGalleryWidth] = useState(100);

  const atLeastOneSetToShow = totalResults > 0;

  if (isLoading || isFetching) {
    return (
      <>
        <GalleryControls
          items={sets}
          first={first}
          page={page}
          setCardsPerRow={setSetsPerRow}
          setFirst={setFirst}
          setGalleryWidth={setGalleryWidth}
          setPage={setPage}
          setSkip={setSkip}
          skip={skip}
          totalResults={totalResults}
          cardsPerRow={setsPerRow}
          galleryType="sets"
          isLoading={isLoading}
          isFetching={isFetching}
        />{' '}
        <SetGalleryWrapper setsPerRow={setsPerRow} galleryWidth={galleryWidth}>
          {Array.from({ length: first }, (_, i) => (
            <Skeleton key={i} variant="rect" height={userId ? 500 : 422} animation="wave" />
          ))}
        </SetGalleryWrapper>
      </>
    );
  }
  return atLeastOneSetToShow ? (
    <>
      <GalleryControls
        items={sets}
        first={first}
        page={page}
        setCardsPerRow={setSetsPerRow}
        setFirst={setFirst}
        setGalleryWidth={setGalleryWidth}
        setPage={setPage}
        setSkip={setSkip}
        skip={skip}
        totalResults={totalResults}
        cardsPerRow={setsPerRow}
        isFetching={isFetching}
        galleryType="sets"
      />

      <SetGalleryWrapper setsPerRow={setsPerRow} galleryWidth={galleryWidth}>
        {sets &&
          sets.map((set) => {
            const costsToPurchaseInSet = costsToPurchase?.find((costs) => Number(costs.setId) === Number(set.id));
            return (
              <SetBox
                key={`set-box-${set.id}`}
                set={set}
                costsToPurchaseInSet={costsToPurchaseInSet}
                priceType={priceType}
                userId={userId}
              />
            );
          })}
      </SetGalleryWrapper>

      <div style={{ marginTop: '20px' }}>
        <GalleryControls
          items={sets}
          first={first}
          page={page}
          setCardsPerRow={setSetsPerRow}
          setFirst={setFirst}
          setGalleryWidth={setGalleryWidth}
          setPage={setPage}
          setSkip={setSkip}
          skip={skip}
          totalResults={totalResults}
          cardsPerRow={setsPerRow}
          isFetching={isFetching}
          galleryType="sets"
          isOnBottom
        />
      </div>
    </>
  ) : (
    <Grid container alignItems="center" justify="center">
      <Grid item>
        <Typography variant="h6">No results found -- try another search!</Typography>
      </Grid>
    </Grid>
  );
};

interface SetGalleryWrapperProps {
  setsPerRow: number;
  galleryWidth: number;
}

const SetGalleryWrapper = styled.div<SetGalleryWrapperProps>(({ setsPerRow = 4, galleryWidth = 100, theme }) => ({
  display: 'grid',
  [theme.breakpoints.up('xs')]: {
    gridTemplateColumns: `repeat(1, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('sm')]: {
    gridTemplateColumns: `repeat(2, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('md')]: {
    gridTemplateColumns: `repeat(2, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('lg')]: {
    gridTemplateColumns: `repeat(3, minmax(0, 1fr))`,
  },
  [theme.breakpoints.up('xl')]: {
    gridTemplateColumns: `repeat(${setsPerRow}, minmax(0, 1fr))`,
  },

  gridTemplateRows: 'repeat(1, 1fr)',
  gap: `10px 10px`,
  width: `${galleryWidth}%`,
  margin: '0 auto',
}));

interface SetGalleryProps {
  sets: Set[];
  costsToPurchase: SetSummary[];
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  totalResults: number;
  priceType: PriceTypes;
  userId?: string;
  isLoading: boolean;
  isFetching: boolean;
}

export default SetGallery;
