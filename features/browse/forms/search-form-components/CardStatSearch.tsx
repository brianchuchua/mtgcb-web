import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import Select from '@material-ui/core/Select';
import ButtonGroup from '@material-ui/core/ButtonGroup';
import Button from '@material-ui/core/Button';
import AddCircleIcon from '@material-ui/icons/AddBox';
import RemoveIcon from '@material-ui/icons/IndeterminateCheckBox';
import MenuItem from '@material-ui/core/MenuItem';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import FormControl from '@material-ui/core/FormControl';
import { RootState } from '../../../../redux/rootReducer';
import {
  ColorTypes,
  SearchAttribute,
  SearchComparators,
  setSearchAttribute,
  setComparator,
  setCardStatSearches,
  addCardStatSearch,
  removeCardStatSearch,
  searchAttributeOptions,
} from '../../browseSlice';

const CardStatSearch: React.FC = () => {
  const dispatch = useDispatch();
  const { cardStatSearches } = useSelector((state: RootState) => state.browse);

  const updateSearchAttribute = (searchAttribute: SearchAttribute, index: number) => {
    const payload = { searchAttribute, index };
    dispatch(setSearchAttribute(payload));
  };

  const updateComparator = (comparator: SearchComparators, index: number) => {
    const payload = { comparator, index };
    dispatch(setComparator(payload));
  };

  const updateCardStatSearches = (value: string, index: number) => {
    const payload = { value, index };
    dispatch(setCardStatSearches(payload));
  };

  const handleAddCardStatSearch = () => {
    dispatch(addCardStatSearch());
  };

  const handleRemoveCardStatSearch = () => {
    dispatch(removeCardStatSearch());
  };

  const handleSearchAttributeChange = (event: React.ChangeEvent<{ value: unknown }>, index: number) => {
    updateSearchAttribute(event.target.value as SearchAttribute, index);
  };

  const handleComparatorChange = (event: React.ChangeEvent<{ value: unknown }>, index: number) => {
    updateComparator(event.target.value as SearchComparators, index);
  };

  const handleCardStatSearchChange = (event: React.ChangeEvent<{ value: unknown }>, index: number) => {
    updateCardStatSearches(event.target.value as ColorTypes, index);
  };

  return (
    <CardStatSearchGroup>
      {cardStatSearches.map((cardStatSearch, index) => (
        <div key={index}>
          <CardStatAttributeSelect
            value={cardStatSearch.searchAttribute}
            defaultValue="convertedManaCost"
            onChange={(event) => handleSearchAttributeChange(event, index)}
            variant="outlined"
          >
            {searchAttributeOptions.map((searchAttribute) => (
              <MenuItem key={searchAttribute.label} value={searchAttribute.value}>
                {searchAttribute.label}
              </MenuItem>
            ))}
          </CardStatAttributeSelect>
          <CardStatComparatorSelect
            value={cardStatSearch.comparator}
            defaultValue=">="
            onChange={(event) => handleComparatorChange(event, index)}
            variant="outlined"
          >
            <MenuItem value="gte">{'>='}</MenuItem>
            <MenuItem value="gt">{'>'}</MenuItem>
            <MenuItem value="lte">{'<='}</MenuItem>
            <MenuItem value="lt">{'<'}</MenuItem>
            <MenuItem value="eq">=</MenuItem>
            <MenuItem value="not">!=</MenuItem>
          </CardStatComparatorSelect>
          <CardStatValue variant="outlined">
            <OutlinedInput
              value={cardStatSearch.value}
              placeholder="1, 2, 3"
              onChange={(event) => handleCardStatSearchChange(event, index)}
            />
          </CardStatValue>
        </div>
      ))}
      <ButtonGroup style={{ display: 'flex', justifyContent: 'flex-end' }}>
        <Button size="small" onClick={handleRemoveCardStatSearch}>
          <RemoveIcon />
        </Button>
        <Button size="small" onClick={handleAddCardStatSearch}>
          <AddCircleIcon />
        </Button>
      </ButtonGroup>
    </CardStatSearchGroup>
  );
};

const CardStatSearchGroup = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

const CardStatValue = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
  width: '30%',
}));

const CardStatAttributeSelect = styled(Select)(() => ({
  width: '45%',
  marginRight: '8px',
}));

const CardStatComparatorSelect = styled(Select)(() => ({
  width: '25%',
}));

export default CardStatSearch;
