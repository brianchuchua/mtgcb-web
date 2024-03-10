import Button from '@material-ui/core/Button';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../redux/rootReducer';
import { useResetQueryParameters } from '../../../util/useResetQueryParameters';
import {
  ArtistSearch,
  CardNameSearch,
  CardStatSearch,
  ColorSelector,
  IncludeSubsetGroupsToggle,
  IncludeSubsetsToggle,
  OracleTextSearch,
  PriceTypeSelector,
  RaritySelector,
  SetCategorySelector,
  SetNameSearch,
  SetSelector,
  SetSortSelector,
  SetTypeSelector,
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
  setCardSets,
  setCardSort,
  setCardSortDirection,
  setCardStatSearches,
  setCardTypes,
  setColorType,
  setComparator,
  setExpansionCategories,
  setExpansionSearchQuery,
  setExpansionSort,
  setExpansionSortDirection,
  setExpansionTypes,
  setIncludeSubsetGroups,
  setIncludeSubsets,
  setOracleTextQuery,
  setPriceType,
  setSearchAttribute,
  setSearchQuery,
  setViewMode,
  setViewSubject,
} from '../browseSlice';

// TODO: Add headers to this thing
const SearchForm: React.FC = () => {
  const { isFormVisible, viewSubject } = useSelector((state: RootState) => state.browse);
  const reduxSlice = 'browse';
  const dispatch = useDispatch();
  const resetQueryParameters = useResetQueryParameters();

  return (
    <>
      {isFormVisible && viewSubject === 'cards' && (
        <form>
          <ViewModeSelector reduxSlice={reduxSlice} setViewMode={setViewMode} setViewSubject={setViewSubject} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <CardNameSearch reduxSlice={reduxSlice} setSearchQuery={setSearchQuery} />
          <OracleTextSearch reduxSlice={reduxSlice} setOracleTextQuery={setOracleTextQuery} />
          <ArtistSearch reduxSlice={reduxSlice} setArtistQuery={setArtistQuery} />
          <TypeSelector reduxSlice={reduxSlice} setCardTypes={setCardTypes} />
          <ColorSelector reduxSlice={reduxSlice} setCardColors={setCardColors} setColorType={setColorType} />
          <RaritySelector reduxSlice={reduxSlice} setCardRarities={setCardRarities} />
          <SetSelector reduxSlice={reduxSlice} setCardSets={setCardSets} />
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
            <Button
              type="reset"
              size="small"
              fullWidth
              variant="contained"
              color="secondary"
              onClick={() => {
                dispatch(reset());
                resetQueryParameters();
              }}
            >
              Reset Search
            </Button>
          </ButtonWrapper>
        </form>
      )}
      {isFormVisible && viewSubject === 'sets' && (
        <form>
          <ViewModeSelector reduxSlice={reduxSlice} setViewMode={setViewMode} setViewSubject={setViewSubject} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <SetNameSearch reduxSlice={reduxSlice} setExpansionSearchQuery={setExpansionSearchQuery} />
          <SetCategorySelector reduxSlice={reduxSlice} setExpansionCategories={setExpansionCategories} />
          <SetTypeSelector reduxSlice={reduxSlice} setExpansionTypes={setExpansionTypes} />
          <SetSortSelector
            reduxSlice={reduxSlice}
            setExpansionSort={setExpansionSort}
            setExpansionSortDirection={setExpansionSortDirection}
          />
          <IncludeSubsetGroupsToggle reduxSlice={reduxSlice} setIncludeSubsetGroups={setIncludeSubsetGroups} />
          <IncludeSubsetsToggle reduxSlice={reduxSlice} setIncludeSubsets={setIncludeSubsets} />
          <ButtonWrapper>
            <Button
              type="reset"
              size="small"
              fullWidth
              variant="contained"
              color="secondary"
              onClick={() => {
                dispatch(reset());
                resetQueryParameters();
              }}
            >
              Reset Search
            </Button>
          </ButtonWrapper>
        </form>
      )}
    </>
  );
};

const ButtonWrapper = styled.div({
  paddingLeft: '8px',
  paddingRight: '8px',
});

export default SearchForm;
