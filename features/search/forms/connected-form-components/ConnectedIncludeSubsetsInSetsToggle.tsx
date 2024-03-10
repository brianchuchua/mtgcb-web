import { useDispatch, useSelector } from 'react-redux';
import IncludeSubsetsInSetsToggle from '../../../../components/search-form-components/IncludeSubsetsInSetsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedIncludeSubsetsWithSetsToggleProps extends ConnectedSearchFormComponentProps {
  setIncludeSubsetsInSets: any;
}

const ConnectedIncludeSubsetsInSetsToggle: React.FC<ConnectedIncludeSubsetsWithSetsToggleProps> = ({
  reduxSlice,
  setIncludeSubsetsInSets,
}) => {
  const dispatch = useDispatch();
  const { includeSubsetsInSets } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updateIncludeSubsetsWithSets = (toggleValue) => {
    dispatch(setIncludeSubsetsInSets(toggleValue));
    setQueryParameter('includeSubsetsInSets', toggleValue ? '1' : '0');
  };

  return (
    <IncludeSubsetsInSetsToggle includeSubsetsInSets={includeSubsetsInSets} updateIncludeSubsetsInSets={updateIncludeSubsetsWithSets} />
  );
};

export default ConnectedIncludeSubsetsInSetsToggle;
