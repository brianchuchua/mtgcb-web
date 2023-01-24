import { useDispatch, useSelector } from 'react-redux';
import IncludeSubsetsToggle from '../../../../components/search-form-components/IncludeSubsetsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedIncludeSubsetsToggleProps extends ConnectedSearchFormComponentProps {
  setIncludeSubsets: any;
}

const ConnectedIncludeSubsetsToggle: React.FC<ConnectedIncludeSubsetsToggleProps> = ({ reduxSlice, setIncludeSubsets }) => {
  const dispatch = useDispatch();
  const { includeSubsets } = useSelector((state: RootState) => state[reduxSlice]);

  const updateIncludeSubsets = (toggleValue) => {
    dispatch(setIncludeSubsets(toggleValue));
  };

  return <IncludeSubsetsToggle includeSubsets={includeSubsets} updateIncludeSubsets={updateIncludeSubsets} />;
};

export default ConnectedIncludeSubsetsToggle;
