import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import Select from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import { RootState } from '../../../../redux/rootReducer';
import { sortByOptions, setCardSort, setCardSortDirection } from '../../browseSlice';

const SortSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { sortBy, sortByDirection } = useSelector((state: RootState) => state.browse);

  const handleSortByChange = (event: React.ChangeEvent<{ value: string }>) => {
    dispatch(setCardSort(event.target.value));
  };

  const handleSortByDirectionChange = (event: React.ChangeEvent<{ value: 'ASC' | 'DESC' }>) => {
    dispatch(setCardSortDirection(event.target.value));
  };

  return (
    <StyledSortSelector>
      <SortByOption value={sortBy} onChange={handleSortByChange} variant="outlined">
        {sortByOptions.map((sortByOption) => (
          <MenuItem key={sortByOption.label} value={sortByOption.value}>
            Sort by {sortByOption.label}
          </MenuItem>
        ))}
      </SortByOption>
      <SortByDirection value={sortByDirection} onChange={handleSortByDirectionChange} variant="outlined">
        <MenuItem value="ASC">ASC</MenuItem>
        <MenuItem value="DESC">DESC</MenuItem>
      </SortByDirection>
    </StyledSortSelector>
  );
};

const StyledSortSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
  width: '100%',
}));

const SortByOption = styled(Select)(() => ({
  width: '65%',
  marginRight: '8px',
}));

const SortByDirection = styled(Select)(() => ({
  width: 'calc(35% - 8px)',
}));

export default SortSelector;
