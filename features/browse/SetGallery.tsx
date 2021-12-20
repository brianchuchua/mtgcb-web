import Grid from '@material-ui/core/Grid';
import Typography from '@material-ui/core/Typography';
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
}) => {
  const [setsPerRow, setSetsPerRow] = useState(4);
  const [galleryWidth, setGalleryWidth] = useState(100);

  const atLeastOneSetToShow = totalResults > 0;

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
        galleryType="sets"
      />

      <SetGalleryWrapper setsPerRow={setsPerRow} galleryWidth={galleryWidth}>
        {sets &&
          sets.map((set) => {
            const costsToPurchaseInSet = costsToPurchase?.find((costs) => Number(costs.setId) === Number(set.id));
            return <SetBox key={`set-box-${set.id}`} set={set} costsToPurchaseInSet={costsToPurchaseInSet} priceType={priceType} />;
          })}
      </SetGalleryWrapper>
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

const SetGalleryWrapper = styled.div<SetGalleryWrapperProps>(({ setsPerRow = 4, galleryWidth = 100 }) => ({
  display: 'grid',
  gridTemplateColumns: `repeat(${setsPerRow}, minmax(0, 1fr))`,
  gridTemplateRows: 'repeat(3, 1fr)',
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
}

export default SetGallery;
