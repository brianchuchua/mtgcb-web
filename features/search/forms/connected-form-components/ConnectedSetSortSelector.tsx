import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/Select';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../../redux/rootReducer';
import { expansionSortByOptions, expansionSortByOptionsForCollectors } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetSortSelectorProps extends ConnectedSearchFormComponentProps {
  setExpansionSort: any;
  setExpansionSortDirection: any;
}

const ConnectedSetSortSelector: React.FC<ConnectedSetSortSelectorProps> = ({ reduxSlice, setExpansionSort, setExpansionSortDirection }) => {
  const dispatch = useDispatch();
  const { sortExpansionBy, sortExpansionByDirection } = useSelector((state: RootState) => state[reduxSlice]);

  const handleSortByChange = (event: React.ChangeEvent<{ value: string }>) => {
    dispatch(setExpansionSort(event.target.value));
  };

  const handleSortByDirectionChange = (event: React.ChangeEvent<{ value: 'ASC' | 'DESC' }>) => {
    dispatch(setExpansionSortDirection(event.target.value));
  };

  const isCollectorMode = reduxSlice === 'collection' || reduxSlice === 'setCollection';
  const sortOptions = !isCollectorMode ? expansionSortByOptions : expansionSortByOptions.concat(expansionSortByOptionsForCollectors);
  return (
    <StyledSortSelector>
      <SortByOption value={sortExpansionBy} onChange={handleSortByChange} variant="outlined">
        {sortOptions.map((sortByOption) => (
          <MenuItem key={sortByOption.label} value={sortByOption.value}>
            Sort by {sortByOption.label}
          </MenuItem>
        ))}
      </SortByOption>
      <SortByDirection value={sortExpansionByDirection} onChange={handleSortByDirectionChange} variant="outlined">
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

export default ConnectedSetSortSelector;
