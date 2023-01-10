import Button from '@material-ui/core/Button';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../redux/rootReducer';
import {
  ArtistSearch,
  CardNameSearch,
  CardStatSearch,
  ColorSelector,
  OracleTextSearch,
  PriceTypeSelector,
  RaritySelector,
  SortSelector,
  TypeSelector,
  ViewModeSelector,
} from '../../search/forms/connected-form-components';
import {
  addCardStatSearch,
  removeCardStatSearch,
  reset,
  setArtistQuery,
  setCardColors,
  setCardRarities,
  setCardSort,
  setCardSortDirection,
  setCardStatSearches,
  setCardTypes,
  setColorType,
  setComparator,
  setOracleTextQuery,
  setPriceType,
  setSearchAttribute,
  setSearchQuery,
  setViewMode,
  setViewSubject,
} from '../setSlice';

// TODO: Add headers to this thing
const SetSearchForm: React.FC = () => {
  const { isFormVisible, viewSubject } = useSelector((state: RootState) => state.set);
  const reduxSlice = 'set';
  const dispatch = useDispatch();

  return (
    <>
      {isFormVisible && viewSubject === 'cards' && (
        <>
          <ViewModeSelector
            showSubjectChangeSection={false}
            reduxSlice={reduxSlice}
            setViewMode={setViewMode}
            setViewSubject={setViewSubject}
          />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <CardNameSearch reduxSlice={reduxSlice} setSearchQuery={setSearchQuery} />
          <OracleTextSearch reduxSlice={reduxSlice} setOracleTextQuery={setOracleTextQuery} />
          <ArtistSearch reduxSlice={reduxSlice} setArtistQuery={setArtistQuery} />
          <TypeSelector reduxSlice={reduxSlice} setCardTypes={setCardTypes} />
          <ColorSelector reduxSlice={reduxSlice} setColorType={setColorType} setCardColors={setCardColors} />
          <RaritySelector reduxSlice={reduxSlice} setCardRarities={setCardRarities} />
          <CardStatSearch
            reduxSlice={reduxSlice}
            addCardStatSearch={addCardStatSearch}
            removeCardStatSearch={removeCardStatSearch}
            setCardStatSearches={setCardStatSearches}
            setComparator={setComparator}
            setSearchAttribute={setSearchAttribute}
          />
          <SortSelector reduxSlice={reduxSlice} setCardSort={setCardSort} setCardSortDirection={setCardSortDirection} />
          <ButtonWrapper>
            <Button size="small" fullWidth variant="contained" color="secondary" onClick={() => dispatch(reset())}>
              Reset Search
            </Button>
          </ButtonWrapper>
        </>
      )}
    </>
  );
};

const ButtonWrapper = styled.div({
  paddingLeft: '8px',
  paddingRight: '8px',
});
export default SetSearchForm;
