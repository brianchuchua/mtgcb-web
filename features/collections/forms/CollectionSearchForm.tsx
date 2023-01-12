import Button from '@material-ui/core/Button';
import Collapse from '@material-ui/core/Collapse';
import FormLabel from '@material-ui/core/FormLabel';
import { useState } from 'react';
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
  QuantitySelector,
  RaritySelector,
  SetCategorySelector,
  SetCompletionStatusSelector,
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
  searchAttributeOptions,
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
  setOracleTextQuery,
  setPriceType,
  setQuantityAll,
  setQuantityFoil,
  setQuantityNormal,
  setSearchAttribute,
  setSearchQuery,
  setSetCompletionStatuses,
  setViewMode,
  setViewSubject,
  sortByOptions,
} from '../collectionSlice';

// TODO: Add headers to this thing
const CollectionSearchForm: React.FC = () => {
  const { isFormVisible, viewSubject } = useSelector((state: RootState) => state.collection);
  const reduxSlice = 'collection';
  const dispatch = useDispatch();

  const [showAdvancedQuantityOptions, setShowAdvancedQuantityOptions] = useState(false);

  return (
    <>
      {isFormVisible && viewSubject === 'cards' && (
        <>
          <ViewModeSelector reduxSlice={reduxSlice} setViewMode={setViewMode} setViewSubject={setViewSubject} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <CardNameSearch reduxSlice={reduxSlice} setSearchQuery={setSearchQuery} />
          <OracleTextSearch reduxSlice={reduxSlice} setOracleTextQuery={setOracleTextQuery} />
          <ArtistSearch reduxSlice={reduxSlice} setArtistQuery={setArtistQuery} />
          <TypeSelector reduxSlice={reduxSlice} setCardTypes={setCardTypes} />
          <ColorSelector reduxSlice={reduxSlice} setColorType={setColorType} setCardColors={setCardColors} />
          <RaritySelector reduxSlice={reduxSlice} setCardRarities={setCardRarities} />
          <SetSelector reduxSlice={reduxSlice} setCardSets={setCardSets} />
          <QuantitySelector
            reduxSlice={reduxSlice}
            setQuantity={setQuantityAll}
            quantityType="quantityAll"
            label="Quantity Collected (All)"
          />
          <StyledFormLabel onClick={() => setShowAdvancedQuantityOptions(!showAdvancedQuantityOptions)}>
            (Click to {showAdvancedQuantityOptions ? 'hide' : 'show'} more quantity options)
          </StyledFormLabel>
          <Collapse in={showAdvancedQuantityOptions}>
            <QuantitySelector
              reduxSlice={reduxSlice}
              setQuantity={setQuantityNormal}
              quantityType="quantityNormal"
              label="Quantity Collected (Normal)"
            />
            <QuantitySelector
              reduxSlice={reduxSlice}
              setQuantity={setQuantityFoil}
              quantityType="quantityFoil"
              label="Quantity Collected (Foil)"
            />
          </Collapse>
          <CardStatSearch
            reduxSlice={reduxSlice}
            addCardStatSearch={addCardStatSearch}
            removeCardStatSearch={removeCardStatSearch}
            setCardStatSearches={setCardStatSearches}
            setComparator={setComparator}
            setSearchAttribute={setSearchAttribute}
            searchAttributeOptionsOverride={searchAttributeOptions}
          />
          <SortSelector
            reduxSlice={reduxSlice}
            sortByOptions={sortByOptions}
            setCardSort={setCardSort}
            setCardSortDirection={setCardSortDirection}
          />
          <ButtonWrapper>
            <Button size="small" fullWidth variant="contained" color="secondary" onClick={() => dispatch(reset())}>
              Reset Search
            </Button>
          </ButtonWrapper>
        </>
      )}
      {isFormVisible && viewSubject === 'sets' && (
        <>
          <ViewModeSelector reduxSlice={reduxSlice} setViewMode={setViewMode} setViewSubject={setViewSubject} />
          <PriceTypeSelector reduxSlice={reduxSlice} setPriceType={setPriceType} />
          <SetCompletionStatusSelector reduxSlice={reduxSlice} setCompletionStatuses={setSetCompletionStatuses} />
          <SetNameSearch reduxSlice={reduxSlice} setExpansionSearchQuery={setExpansionSearchQuery} />
          <SetCategorySelector reduxSlice={reduxSlice} setExpansionCategories={setExpansionCategories} />
          <SetTypeSelector reduxSlice={reduxSlice} setExpansionTypes={setExpansionTypes} />
          <SetSortSelector
            reduxSlice={reduxSlice}
            setExpansionSort={setExpansionSort}
            setExpansionSortDirection={setExpansionSortDirection}
          />
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

const StyledFormLabel = styled(FormLabel)({
  textAlign: 'center',
  paddingBottom: '8px',
  display: 'block',
  fontSize: '0.8rem',
  opacity: 0.5,
  cursor: 'pointer',
  '&:hover': {
    textDecoration: 'underline',
  },
});

export default CollectionSearchForm;
